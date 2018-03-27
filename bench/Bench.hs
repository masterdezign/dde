import           Criterion.Main
import           Linear.V3 ( V3 (..) )
import qualified Data.Vector.Storable as V

import qualified R1
import qualified R2

runTest1 :: Int -> V.Vector (V3 Double)
runTest1 = R1.model

runTest2 :: Int -> V.Vector (V3 Double)
runTest2 = R2.model

main = defaultMain [
    bench "old version" $ whnf runTest1 maxTime
    , bench "target to optimize" $ whnf runTest2 maxTime
  ]
  where
    maxTime = 20  -- No of delays
