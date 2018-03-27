{-# LANGUAGE FlexibleContexts #-}
module Impl2 ( mgModel ) where

import           Linear.V1
import           Numeric.DDE
import           Numeric.DDE.Model
import qualified Data.Vector.Storable as V

parMG0 :: MackeyGlass
parMG0 = MackeyGlass { _beta = 0.2
                     , _gamma = 0.1
                     }

-- | The Mackey-Glass model (with no external input).
-- Typical hStep = 0.1
mgModel :: Double -> Int -> (V1 Double, V.Vector (V1 Double))
mgModel hStep totalIters = r
  where
    -- Initial state x(t0) = 0.2
    state0 = V1 0.2
    len1 = 17 * round (recip hStep)  -- tauD = 17, delay time
    -- Initial conditions
    hist0 = V.replicate len1 state0
    inp = Input (V.replicate (totalIters + 1) 0.0)
    rhs' = mackeyGlassRhs parMG0
    -- Stepper implements Runge-Kutta schema
    stepper = rk4 hStep rhs'
    -- Provide the last state and the time trace
    r = integ' stepper len1 totalIters totalIters (state0, hist0, inp)
