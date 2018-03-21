import           Criterion.Main
import           Linear.V1 ( V1 (..) )
import qualified Data.Vector.Storable as V

import qualified Impl1
import qualified Impl2

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

main = defaultMain [
    bench "old version" $ whnf runTest1 maxTime
    , bench "target to optimize" $ whnf runTest2 maxTime
  ]
  where
    maxTime = 1000  -- 1000 time units
