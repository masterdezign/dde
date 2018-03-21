{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Linear.V1
import           Numeric.DDE
import           Numeric.DDE.Model
import qualified Data.Vector.Storable as V

parMG0 :: MackeyGlass
parMG0 = MackeyGlass { _beta = 0.2
                     , _gamma = 0.1
                     }

-- | The Mackey-Glass model (with no external input).
-- This example demonstrates how to set custom initial conditions.
-- Typical hStep = 0.1
mgModel :: Double -> Int -> V.Vector (V1 Double)
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
    stepper = let (Stepper _rk4) = rk4
              in _rk4 hStep rhs'
    -- Record all the time trace
    (_, r) = integ' stepper len1 totalIters totalIters (state0, hist0, inp)

-- | Comparison with the output.dat produced by:
-- > $ xppaut -silent mg.ode
--
-- > $ cat mg.ode
--
--     x'=beta*delay(x,tau)/(1 + delay(x,tau)^10)-gamma*x
--
--     par tau=17
--     par gamma=0.1
--     par beta=0.2
--
--     x(0)=0.2
--     init x=0.2
--
--     @ total=1000
--     @ bounds=10000
--     # Maximal delay buffer (in time units)
--     @ delay=100
--     @ dt=0.1
--     @ trans=0
--     @ maxstore=100000000
--     done
runTest :: V.Vector Double
runTest =
  let hStep = 0.1
      total = round(1000 / hStep)  -- 1000 time units
      trace = mgModel hStep total
  in V.map (\(V1 x) -> x) trace

main :: IO ()
main = putStrLn $ toString runTest
  where
    toString = unlines. map show. V.toList
