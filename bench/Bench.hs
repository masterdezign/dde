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
-- time                 3.644 ms   (3.595 ms .. 3.697 ms)
--                      0.999 R²   (0.998 R² .. 0.999 R²)
-- mean                 3.677 ms   (3.645 ms .. 3.715 ms)
-- std dev              106.5 μs   (86.91 μs .. 130.9 μs)
-- variance introduced by outliers: 12% (moderately inflated)
--
-- benchmarking dde library version (2D case with external forcing)
-- time                 6.118 ms   (6.026 ms .. 6.200 ms)
--                      0.999 R²   (0.998 R² .. 0.999 R²)
-- mean                 6.241 ms   (6.186 ms .. 6.336 ms)
-- std dev              201.6 μs   (150.1 μs .. 275.6 μs)
-- variance introduced by outliers: 13% (moderately inflated)
