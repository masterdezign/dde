{-# LANGUAGE BangPatterns #-}
module R2 ( model ) where

import           Linear ( V2 (..) )
import qualified Data.Vector.Storable as V
import qualified Numeric.DDE as DDE

rhs :: DDE.RHS (V2 Double)
rhs = DDE.RHS deriv
  where
    deriv (V2 !x !y, DDE.Hist (V2 !x_tau _), DDE.Inp !inp) = V2 x' y'
      where
        tau = 2.15625
        delta = -3.0

        f x = if x < 0 then -x else 0

        x' = (-x - delta * y + f (x_tau + inp)) / tau
        y' = x
        -- No input amplification, i.e. rho = 1 in rho * inp

model' :: Double -> Int -> V.Vector Double -> (V2 Double, V.Vector (V2 Double))
model' dt len1 inp = res
  where
    -- Initial conditions:
    -- dynamical state and delay history.
    state0 = V2 0.0 0.0
    hist0 = V.replicate len1 state0

    inp' = DDE.Input inp

    res = DDE.integ DDE.heun2 state0 hist0 len1 dt rhs inp'

model :: Double -> Int -> V.Vector Double -> V.Vector (V2 Double)
model dt delaySamples inp =
  let (state1, traces) = model' dt delaySamples inp
  in traces
