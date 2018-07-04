import           Criterion.Main
import           Linear.V1 ( V1 (..) )
import           Linear.V2 ( V2 (..) )
import qualified Data.Vector.Storable as V

import qualified Impl1
import qualified Impl2
import qualified R1
import qualified R2

runTest1 :: Double -> Double
runTest1 maxTime =
  let hStep = 0.1
      total = round(maxTime / hStep)
      s = Impl1.mgModel hStep total
  in s

runTest2 :: Double -> Double
runTest2 maxTime =
  let hStep = 0.1
      total = round(maxTime / hStep)
      (V1 s, _) = Impl2.mgModel hStep total
  in s

runTest3 :: V.Vector Double -> Double
runTest3 inp =
  let hStep = recip 256.0
      delaySamples = 800
      result = R1.model hStep delaySamples inp
  in V.last result

runTest4 :: V.Vector Double -> Double
runTest4 inp =
  let hStep = recip 256.0
      delaySamples = 800
      V2 r _ = V.last $ R2.model hStep delaySamples inp
  in r

main = defaultMain [
    bench "hard coded version" $ whnf runTest1 maxTime
    , bench "dde library version" $ whnf runTest2 maxTime

    , bench "hard coded version (2D case with external forcing)" $ whnf runTest3 inp
    , bench "dde library version (2D case with external forcing)" $ whnf runTest4 inp
  ]
  where
    maxTime = 1000  -- 1000 time units

    total = round(maxTime * 256)
    inp = V.fromList $ map (sin. (0.001*pi*). fromIntegral) [1..total]


-- Benchmark was run on Dell Precision T3610
--
-- time                 6.377 ms   (6.326 ms .. 6.430 ms)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 6.433 ms   (6.397 ms .. 6.481 ms)
-- std dev              125.5 μs   (88.80 μs .. 170.0 μs)

-- benchmarking dde library version
-- time                 6.570 ms   (6.559 ms .. 6.581 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 6.549 ms   (6.538 ms .. 6.557 ms)
-- std dev              26.70 μs   (22.69 μs .. 31.91 μs)

-- benchmarking hard coded version (2D case with external forcing)
-- time                 3.641 ms   (3.604 ms .. 3.695 ms)
--                      0.998 R²   (0.996 R² .. 1.000 R²)
-- mean                 3.659 ms   (3.640 ms .. 3.693 ms)
-- std dev              74.47 μs   (49.20 μs .. 124.5 μs)

-- benchmarking dde library version (2D case with external forcing)
-- time                 5.964 ms   (5.839 ms .. 6.067 ms)
--                      0.998 R²   (0.997 R² .. 0.999 R²)
-- mean                 5.958 ms   (5.928 ms .. 5.989 ms)
-- std dev              96.24 μs   (73.59 μs .. 144.5 μs)
