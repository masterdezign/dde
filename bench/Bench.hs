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
      delaySamples = 100
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

    total = round(20 * maxTime * 256)
    inp = V.fromList $ map (sin. (0.001*pi*). fromIntegral) [1..total]

-- benchmarking hard coded version
-- time                 6.546 ms   (6.471 ms .. 6.627 ms)
--                      0.998 R²   (0.996 R² .. 0.999 R²)
-- mean                 6.755 ms   (6.673 ms .. 6.871 ms)
-- std dev              272.1 μs   (193.4 μs .. 376.0 μs)
-- variance introduced by outliers: 18% (moderately inflated)
--
-- benchmarking dde library version
-- time                 6.860 ms   (6.786 ms .. 6.935 ms)
--                      0.998 R²   (0.996 R² .. 1.000 R²)
-- mean                 6.843 ms   (6.796 ms .. 6.917 ms)
-- std dev              163.7 μs   (112.2 μs .. 262.5 μs)
--
-- benchmarking hard coded version (2D case with external forcing)
-- time                 73.81 ms   (71.72 ms .. 74.75 ms)
--                      0.999 R²   (0.996 R² .. 1.000 R²)
-- mean                 74.67 ms   (73.78 ms .. 76.50 ms)
-- std dev              2.075 ms   (849.9 μs .. 3.039 ms)
--
-- benchmarking dde library version (2D case with external forcing)
-- time                 101.6 ms   (97.54 ms .. 105.1 ms)
--                      0.998 R²   (0.995 R² .. 1.000 R²)
-- mean                 105.5 ms   (103.0 ms .. 112.2 ms)
-- std dev              6.021 ms   (1.470 ms .. 9.635 ms)
-- variance introduced by outliers: 10% (moderately inflated)
