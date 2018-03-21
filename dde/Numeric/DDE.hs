{- |
  = Delay differential equations (DDE)


  == Example: Ikeda DDE

  Below is a complete example simulating the Ikeda DDE defined as:
  @tau * x(t)/dt = -x + beta * sin[x(t - tau_D)]@.

  > import           Linear ( V1 (..) )
  > import qualified Data.Vector.Storable as V
  > import qualified Numeric.DDE as DDE
  >
  > ikedaRhs beta = DDE.RHS derivative
  >   where
  >     derivative ((V1 x), (DDE.Hist (V1 x_tauD)), _) = V1 x'
  >       where
  >         -- Ikeda DDE definition
  >         x' = (-x + beta * (sin x_tauD)) / tau
  >
  >         -- Constants
  >         tau = 0.01
  >
  > model beta hStep len1 totalIter = (state1, V.map (\(V1 x) -> x) trace)
  >   where
  >     -- Initial conditions:
  >     -- dynamical state and delay history.
  >     state0 = V1 (pi/2)
  >     hist0 = V.replicate len1 state0
  >
  >     -- Input is ignored in ikedaRhs
  >     inp = DDE.Input $ V.replicate (totalIter + 1) 0
  >
  >     (state1, trace) = DDE.integ DDE.rk4 state0 hist0 len1 hStep (ikedaRhs beta) inp
  >
  > -- Control parameter
  > beta = 2.6
  >
  > main = do
  >   let hStep = 0.001  -- Integration step
  >       tauD = 1.0  -- Delay time
  >       samplesPerDelay = round $ tauD / hStep
  >       delays = 8
  >       total = delays * samplesPerDelay
  >
  >   let (state1, trace) = model beta hStep samplesPerDelay total
  >
  >   mapM_ print $ V.toList trace

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Numeric.DDE (
  -- * Integrators
    integ
  , integ'
  , integRk4
  , integHeun2
  , integRk4_2D
  , integHeun2_2D
  , Input (..)
  , InputSnapshot (..)
  , HistorySnapshot (..)

  -- * Steppers
  , RHS (..)
  , Stepper (..)
  , rk4
  , heun2
  ) where

import           Data.VectorSpace.Free
import           Linear ( (^/), V1 (..), V2 (..) )
import           Foreign.Storable ( Storable (..) )
import           System.IO.Unsafe ( unsafePerformIO )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import           Numeric.DDE.Types


-- | Fourth order Runge-Kutta stepper
rk4 :: Stepper
rk4 = Stepper _rk4
  where
    _rk4 dt (RHS rhs') !xy (Hist !xy_tau1, Hist !xy_tau1') (!u1, !u1') = xy_next
      where
        xy_next = xy ^+^ (a ^+^ 2 *^ b ^+^ 2 *^ c ^+^ d) ^/ 6
        a = dt *^ rhs' (xy, Hist xy_tau1, inp1)
        b = dt *^ rhs' (xy ^+^ a ^/ 2, Hist xy_tau1_b, inp1_b)
        c = dt *^ rhs' (xy ^+^ b ^/ 2, Hist xy_tau1_c, inp1_c)
        d = dt *^ rhs' (xy ^+^ c, Hist xy_tau1', inp1')
        xy_tau1_b = (xy_tau1 ^+^ xy_tau1') ^/ 2
        xy_tau1_c = xy_tau1_b
        inp1 = Inp u1
        inp1_b = Inp $ (u1 + u1') / 2
        inp1_c = inp1_b
        inp1' = Inp u1'

-- | Second order Heun's stepper
heun2 :: Stepper
heun2 = Stepper _heun2
  where
    _heun2 hStep (RHS rhs') !xy (!xy_tau1, !xy_tau1') (!u1, !u1') = xy_next
      where
        f1 = rhs' (xy, xy_tau1, Inp u1)
        xy_ = xy ^+^ hStep *^ f1
        f2 = rhs' (xy_, xy_tau1', Inp u1')
        xy_next = xy ^+^ (hStep *^ (f1 ^+^ f2)) ^/ 2.0

-- | Generic integrator for DDEs (single delay time).
-- Records all dynamical variables.
--
integ'
  :: Storable state
  => (state -> (HistorySnapshot state, HistorySnapshot state) -> (Double, Double) -> state)
  -- ^ Iterator describing a DDE system
  -> Int
  -- ^ Delay length in samples
  -> Int
  -- ^ Number of last samples to record
  -> Int
  -- ^ Total number of iterations
  -> (state, V.Vector state, Input)
  -- ^ Initial state vector, initial history, and external forcing
  -> (state, V.Vector state)
  -- ^ Final state and recorded state of the first variable.
  -- The latter is a vector of vectors (matrix) when multiple variables are involved.
integ' iter1 len1 krecord total (!xy0, !hist0, Input !in1) = a
  where
    a = unsafePerformIO $ do
      ! v <- VM.new (len1 + total)  -- Delay history
      -- Copy the initial history values
      copyHist v hist0

      -- Calculate the rest of the vector
      xy' <- go v len1 xy0

      trace <- V.unsafeFreeze v
      return (xy', V.slice (len1 + total - krecord) krecord trace)

    -- Copy initial conditions
    copyHist !v !hist =
      mapM_ (\i -> VM.unsafeWrite v i (hist V.! i)) [0..V.length hist - 1]

    go !v !i !xy
      | i == len1 + total =
          return xy
      | otherwise = do
        xy_tau1 <- VM.unsafeRead v (i - len1)  -- Two subsequent delayed states
        xy_tau1' <- VM.unsafeRead v (i - len1 + 1)
        let u1 = in1 V.! (i - len1)  -- Two subsequent inputs
            u1' = in1 V.! (i - len1 + 1)
            !xy' = iter1 xy (Hist xy_tau1, Hist xy_tau1') (u1, u1')
        VM.unsafeWrite v i xy'
        go v (i + 1) xy'

-- | Generic integrator that records the whole time trace @x(t)@
-- (single delay time).
integ
  :: (Functor state, Storable (state Double), VectorSpace (state Double), Num (Scalar (state Double)))
  => Stepper
  -> state Double  -- ^ Initial state vector (x(t), y(t),...)
  -> V.Vector (state Double)  -- ^ Initial history for delayed variables
  -> Int  -- ^ Delay length in samples
  -> Scalar (state Double)  -- ^ Integration step
  -> RHS (state Double)  -- ^ Derivative (DDE right-hand side)
  -> Input  -- ^ External forcing
  -> (state Double, V.Vector (state Double))
integ (Stepper stp) state0 hist0 len1 dt rhs' inp@(Input in1) = r
  where
    -- Two subsequent inputs are needed for `rk4` and `heun2`,
    -- therefore subtract one
    ! totalIters = V.length in1 - 1
    ! iterator = stp dt rhs'
    -- Record all the time trace
    ! r = integ' iterator len1 totalIters totalIters (state0, hist0, inp)

-- | RK4 integrator shortcut for 1D DDEs with zero
-- initial conditions
integRk4 :: Int  -- ^ Delay length in samples
         -> Double  -- ^ Integration time step
         -> RHS (V1 Double)  -- ^ DDE model
         -> Input  -- ^ External forcing
         -> (V1 Double, V.Vector (V1 Double))
integRk4 len1 = integ rk4 state0 hist0 len1
  where
    state0 = V1 0.0
    hist0 = V.replicate len1 state0

-- | Shortcut for Heun's 2nd order 1D DDEs with zero
-- initial conditions
integHeun2 :: Int  -- ^ Delay length in samples
           -> Double  -- ^ Integration time step
           -> RHS (V1 Double)  -- ^ DDE model
           -> Input  -- ^ External forcing
           -> (V1 Double, V.Vector (V1 Double))
integHeun2 len1 = integ heun2 state0 hist0 len1
  where
    state0 = V1 0.0
    hist0 = V.replicate len1 state0

-- | RK4 integrator shortcut for 2D DDEs with zero
-- initial conditions
integRk4_2D :: Int  -- ^ Delay length in samples
            -> Double  -- ^ Integration time step
            -> RHS (V2 Double)  -- ^ DDE model
            -> Input  -- ^ External forcing
            -> (V2 Double, V.Vector (V2 Double))
integRk4_2D len1 = integ rk4 state0 hist0 len1
  where
    state0 = V2 0.0 0.0
    hist0 = V.replicate len1 state0

-- | Shortcut for Heun's 2nd order 2D DDEs with zero
-- initial conditions
integHeun2_2D :: Int  -- ^ Delay length in samples
              -> Double  -- ^ Integration time step
              -> RHS (V2 Double)  -- ^ DDE model
              -> Input  -- ^ External forcing
              -> (V2 Double, V.Vector (V2 Double))
integHeun2_2D len1 = integ heun2 state0 hist0 len1
  where
    state0 = V2 0.0 0.0
    hist0 = V.replicate len1 state0
