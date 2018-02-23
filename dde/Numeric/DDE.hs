{- |
    = Delay differential equations (DDE)
-}

{-# LANGUAGE BangPatterns #-}
module Numeric.DDE (
  -- * Integrators
    integRk4
  , integHeun2
  , integ
  , integ'
  , Input (..)
  , State (..)

  -- * Steppers
  , Stepper1 (..)
  , rk4
  , heun2
  ) where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           System.IO.Unsafe ( unsafePerformIO )

import Numeric.DDE.Types

infixl 6 .+.
(.+.) :: V.Vector Double -> V.Vector Double -> V.Vector Double
(.+.) = V.zipWith (+)

infixl 7 *.
(*.) :: Double -> V.Vector Double -> V.Vector Double
(*.) c = V.map (* c)

-- | Fourth order Runge-Kutta stepper
rk4 :: Stepper1
rk4 = Stepper1 _rk4
  where
    _rk4 hStep rhs' (State !xy) !(!x_tau1, !x_tau1') !(!u1, !u1') = State xy_next
      where
        xy_next = xy .+. over6 *. (a .+. 2 *. b .+. 2 *. c .+. d)
        over6 = recip 6
        over2 = recip 2
        ! (State a') = rhs' (State xy, toHist1 x_tau1, inp1)
        ! a = hStep *. a'
        ! (State b') = rhs' (State $ xy .+. over2 *. a, toHist1 x_tau1_b, inp1_b)
        ! b = hStep *. b'
        ! (State c') = rhs' (State $ xy .+. over2 *. b, toHist1 x_tau1_c, inp1_c)
        ! c = hStep *. c'
        ! (State d') = rhs' (State $ xy .+. c, toHist1 x_tau1', inp1')
        ! d = hStep *. d'
        ! x_tau1_b = (x_tau1 + x_tau1') / 2
        ! x_tau1_c = x_tau1_b
        ! inp1 = Inp u1
        ! inp1_b = Inp $ (u1 + u1') / 2
        ! inp1_c = inp1_b
        ! inp1' = Inp u1'

toHist1 :: Double -> HistorySnapshot
toHist1 = Hist. V.singleton

-- | Second order Heun's stepper
heun2 :: Stepper1
heun2 = Stepper1 _heun2
  where
    _heun2 hStep rhs' (State !xy) !(!x_tau1, !x_tau1') !(!u1, !u1') = State xy_next
      where
        ! (State f1) = rhs' (State xy, toHist1 x_tau1, Inp u1)
        ! xy' = xy .+. hStep *. f1
        ! (State f2) = rhs' (State xy', toHist1 x_tau1', Inp u1')
        ! xy_next = xy .+. (hStep / 2.0) *. (f1 .+. f2)

-- | Generic integrator for DDEs with a single delay
integ
  :: (State -> (Double, Double) -> (Double, Double) -> State)
  -- ^ A stepper function describing a DDE system
  -> Int
  -- ^ Delay length in samples
  -> Int
  -- ^ Number of last samples to record
  -> Int
  -- ^ Total number of iterations
  -> (State, V.Vector Double, Input)
  -- ^ Initial state vector, initial history, and external forcing
  -> (State, V.Vector Double)
  -- ^ Final state and recorded state of the first variable.
  -- The latter could be a Matrix if multiple variables are needed
integ iterate1 len1 krecord total (!xy0, !hist0, !(Input input1)) = a
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
    copyHist !v !hist = do
      mapM_ (\i -> VM.unsafeWrite v i (hist V.! i)) [0..(V.length hist) - 1]

    go !v !i !xy
      | i == len1 + total = do
          return xy
      | otherwise = do
        x_tau1 <- VM.unsafeRead v (i - len1)  -- Delayed element
        x_tau1' <- VM.unsafeRead v (i - len1 + 1)  -- The next one
        let u1 = input1 V.! (i - len1)  -- Read two subsequent inputs
            u1' = input1 V.! (i - len1 + 1)
            !xy' = iterate1 xy (x_tau1, x_tau1') (u1, u1')
            !(State xy'1) = xy'
            !x' = xy'1 V.! 0  -- Read x(t) variable
        VM.unsafeWrite v i x'
        go v (i + 1) xy'

-- | Integrator for two-dimensional DDEs with zero
-- initial conditions that records the whole time trace $x(t)$.
integ'
  :: Stepper1
  -> Double
  -> RHS
  -> Int  -- ^ Delay length in samples
  -> Input  -- ^ External forcing
  -> (State, V.Vector Double)
integ' (Stepper1 stp) hStep rhs' len1 inp@(Input in1) = res
  where ! state0 = State (V.replicate 2 0.0)
        ! hist0 = V.replicate len1 0
        -- Two subsequent inputs are needed for `rk4`, therefore subtract one
        ! totalIters = V.length in1 - 1
        -- Iterator implements Runge-Kutta schema
        ! iterator = stp hStep rhs'
        -- Record all the time trace
        ! res = integ iterator len1 totalIters totalIters (state0, hist0, inp)

-- | RK4 integrator shortcut for 2D DDEs with zero
-- initial conditions
integRk4 :: Double  -- ^ Integration time step
         -> RHS  -- ^ A model
         -> Int  -- ^ Delay length in samples
         -> Input  -- ^ External forcing
         -> (State, V.Vector Double)
integRk4 = integ' rk4

-- | Heun's 2nd order shortcut for 2D DDEs with zero
-- initial conditions
integHeun2 :: Double  -- ^ Integration time step
           -> RHS  -- ^ A model
           -> Int  -- ^ Delay length in samples
           -> Input  -- ^ External forcing
           -> (State, V.Vector Double)
integHeun2 = integ' heun2
