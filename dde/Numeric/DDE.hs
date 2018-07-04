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
  >     derivative ((V1 x), (DDE.Hist histSnapshots), _) = V1 x'
  >       where
  >         -- Ikeda DDE definition
  >         x' = (-x + beta * (sin x_tauD)) / tau
  >
  >         -- There is only a single delay in our model
  >         V1 x_tauD = head histSnapshots
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
  >     -- Only one delay
  >     delaysInSamples = [len1]
  >     (state1, trace) = DDE.integ DDE.rk4 state0 hist0 delaysInSamples hStep (ikedaRhs beta) inp
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
{-# LANGUAGE RankNTypes #-}
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
  , HistorySnapshots (..)

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
rk4 dt (RHS rhs') xy (Hist xy_tau1, Hist xy_tau1') (u1, u1') = xy_next
      where
        xy_next = xy ^+^ (a ^+^ 2 *^ b ^+^ 2 *^ c ^+^ d) ^/ 6
        a = dt *^ rhs' (xy, Hist xy_tau1, inp1)
        b = dt *^ rhs' (xy ^+^ a ^/ 2, Hist xy_tau1_b, inp1_b)
        c = dt *^ rhs' (xy ^+^ b ^/ 2, Hist xy_tau1_c, inp1_c)
        d = dt *^ rhs' (xy ^+^ c, Hist xy_tau1', inp1')
        xy_tau1_b = zipWith (\xp xq -> (xp ^+^ xq) ^/ 2) xy_tau1 xy_tau1'
        xy_tau1_c = xy_tau1_b
        inp1 = Inp u1
        inp1_b = Inp $ (u1 + u1') / 2
        inp1_c = inp1_b
        inp1' = Inp u1'

-- | Second order Heun's stepper
heun2 :: Stepper
heun2 hStep (RHS rhs') xy (xy_tau1, xy_tau1') (u1, u1') = xy_next
      where
        f1 = rhs' (xy, xy_tau1, Inp u1)
        xy_ = xy ^+^ hStep *^ f1
        f2 = rhs' (xy_, xy_tau1', Inp u1')
        xy_next = xy ^+^ (hStep *^ (f1 ^+^ f2)) ^/ 2.0
{-# INLINE heun2 #-}

-- | Generic integrator for DDEs.
-- Records all dynamical variables.
integ'
  :: Storable state
  => (state -> (HistorySnapshots state, HistorySnapshots state) -> (Double, Double) -> state)
  -- ^ Iterator describing a DDE system
  -> [Int]
  -- ^ Delay lengths in samples
  -> Int
  -- ^ Number of last samples to record
  -> Int
  -- ^ Total number of iterations
  -> (state, V.Vector state, Input)
  -- ^ Initial state vector, initial history, and external forcing
  -> (state, V.Vector state)
  -- ^ Final state and recorded state of the first variable.
  -- The latter is a vector of vectors (matrix) when multiple variables are involved.
integ' iter lengths krecord total (xy0, hist0, Input in1) = a
  where
    len1 = maximum lengths

    a = unsafePerformIO $ do
      -- The longest delay (in samples)

      v <- VM.new (len1 + total)  -- Delay history
      -- Copy the initial history values
      copyHist v hist0

      -- Calculate the rest of the vector
      xy' <- go v len1 xy0

      trace <- V.unsafeFreeze v
      return (xy', V.slice (len1 + total - krecord) krecord trace)

    -- Copy initial conditions
    -- It should be asserted that V.length hist >= len1
    copyHist v hist =
      mapM_ (\i -> VM.unsafeWrite v i (hist V.! i)) [0..V.length hist - 1]

    go !v !i !xy
      | i == len1 + total =
          return xy
      | otherwise = do
        xy_tau1 <- mapM (\len -> VM.unsafeRead v (i - len)) lengths
        -- Note that xy_tau1 are delayed by one (discrete sample) values from xy_tau1'.
        -- Perhaps, memory access could be somehow optimized.
        xy_tau1' <- mapM (\len -> VM.unsafeRead v (i - len + 1)) lengths
        let u1 = in1 V.! (i - len1)  -- Two subsequent scalar inputs
            u1' = in1 V.! (i - len1 + 1)
            xy' = iter xy (Hist xy_tau1, Hist xy_tau1') (u1, u1')
        VM.unsafeWrite v i xy'
        go v (i + 1) xy'
{-# INLINE integ' #-}

-- | Generic integrator that records the whole time trace @x(t)@.
integ
  :: (Functor state, Storable (state Double), VectorSpace (state Double), Num (Scalar (state Double)))
  => Stepper
  -> state Double  -- ^ Initial state vector (x(t), y(t),...)
  -> V.Vector (state Double)  -- ^ Initial history for delayed variables
  -> [Int]  -- ^ Delay lengths in samples
  -> Scalar (state Double)  -- ^ Integration step
  -> RHS (state Double)  -- ^ Derivative (DDE right-hand side)
  -> Input  -- ^ External forcing
  -> (state Double, V.Vector (state Double))
integ stp state0 hist0 len1 dt rhs' inp@(Input in1) = r
  where
    -- Two subsequent inputs are needed for `rk4` and `heun2`,
    -- therefore subtract one
    totalIters = V.length in1 - 1
    iterator = stp dt rhs'
    -- Record all the time trace
    r = integ' iterator len1 totalIters totalIters (state0, hist0, inp)
{-# INLINE integ #-}

-- | RK4 integrator shortcut for 1D DDEs with zero
-- initial conditions
integRk4 :: [Int]  -- ^ Delay lengths in samples
         -> Double  -- ^ Integration time step
         -> RHS (V1 Double)  -- ^ DDE model
         -> Input  -- ^ External forcing
         -> (V1 Double, V.Vector (V1 Double))
integRk4 lengths = integ rk4 state0 hist0 lengths
  where
    state0 = V1 0.0
    len1 = maximum lengths
    hist0 = V.replicate len1 state0

-- | Shortcut for Heun's 2nd order 1D DDEs with zero
-- initial conditions
integHeun2 :: [Int]  -- ^ Delay lengths in samples
           -> Double  -- ^ Integration time step
           -> RHS (V1 Double)  -- ^ DDE model
           -> Input  -- ^ External forcing
           -> (V1 Double, V.Vector (V1 Double))
integHeun2 lengths = integ heun2 state0 hist0 lengths
  where
    state0 = V1 0.0
    len1 = maximum lengths
    hist0 = V.replicate len1 state0

-- | RK4 integrator shortcut for 2D DDEs with zero
-- initial conditions
integRk4_2D :: [Int]  -- ^ Delay lengths in samples
            -> Double  -- ^ Integration time step
            -> RHS (V2 Double)  -- ^ DDE model
            -> Input  -- ^ External forcing
            -> (V2 Double, V.Vector (V2 Double))
integRk4_2D lengths = integ rk4 state0 hist0 lengths
  where
    state0 = V2 0.0 0.0
    len1 = maximum lengths
    hist0 = V.replicate len1 state0

-- | Shortcut for Heun's 2nd order 2D DDEs with zero
-- initial conditions
integHeun2_2D :: [Int]  -- ^ Delay length in samples
              -> Double  -- ^ Integration time step
              -> RHS (V2 Double)  -- ^ DDE model
              -> Input  -- ^ External forcing
              -> (V2 Double, V.Vector (V2 Double))
integHeun2_2D lengths = integ heun2 state0 hist0 lengths
  where
    state0 = V2 0.0 0.0
    len1 = maximum lengths
    hist0 = V.replicate len1 state0
