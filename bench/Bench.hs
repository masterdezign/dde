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
-- time                 7.603 ms   (7.447 ms .. 7.778 ms)
--                      0.998 R²   (0.996 R² .. 0.999 R²)
-- mean                 7.442 ms   (7.348 ms .. 7.507 ms)
-- std dev              235.3 μs   (186.5 μs .. 356.2 μs)
-- variance introduced by outliers: 11% (moderately inflated)
--
-- benchmarking dde library version
-- time                 8.222 ms   (8.062 ms .. 8.368 ms)
--                      0.998 R²   (0.997 R² .. 0.999 R²)
-- mean                 8.061 ms   (7.973 ms .. 8.148 ms)
-- std dev              248.7 μs   (189.3 μs .. 362.8 μs)
-- variance introduced by outliers: 11% (moderately inflated)
--
-- benchmarking hard coded version (2D case with external forcing)
-- time                 4.071 ms   (4.022 ms .. 4.126 ms)
--                      0.998 R²   (0.997 R² .. 0.999 R²)
-- mean                 4.055 ms   (4.020 ms .. 4.096 ms)
-- std dev              126.0 μs   (98.35 μs .. 178.1 μs)
-- variance introduced by outliers: 15% (moderately inflated)
--
-- benchmarking dde library version (2D case with external forcing)
-- time                 18.73 ms   (18.42 ms .. 19.00 ms)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 18.90 ms   (18.67 ms .. 19.37 ms)
-- std dev              754.2 μs   (327.8 μs .. 1.240 ms)
-- variance introduced by outliers: 13% (moderately inflated)
