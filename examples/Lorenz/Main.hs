-- | Lorenz model
--
-- This example ODE model reconstructs the Lorenz attractor
-- with parameters ρ = 28, σ = 10, and β = 8/3.
-- It can be extended to a DDE by modifying the `lorenz` function
-- to read the delayed value(s) and changing @delaysInSamples@
-- constant in the `model` function to indicate the delay length.

-- Exercise:
-- Stabilize unstable periodic orbits.
-- Ref. K. Pyragas, Phys. Lett. A 170, 421 (1992)
-- http://pyragas.pfi.lt/pdffiles/1992/pla92.pdf

import           Linear ( V3 (..) )  -- From `linear` package
import qualified Data.Vector.Storable as V  -- From `vector` package
import qualified Numeric.DDE as DDE  -- From `dde-0.3.0` package

rhs = DDE.RHS lorenz

lorenz ((V3 x y z), (DDE.Hist _), _) = V3 x' y' z'
  where
    x' = sigma * (y - x)
    y' = x * (rho - z) - y
    z' = x * y - beta * z

    rho = 28
    sigma = 10
    beta = 8.0 / 3

model hStep totalIter = (state1, trace)
  where
    -- Initial conditions
    state0 = V3 1.0 1.0 1.0
    hist0 = V.empty

    -- Input is ignored in `rhs`
    inp = DDE.Input $ V.replicate (totalIter + 1) 0

    -- No delay
    delaysInSamples = [0]

    (state1, trace) = DDE.integ DDE.rk4 state0 hist0 delaysInSamples hStep rhs inp

main = do
  let
    hStep = 0.01  -- Integration time step
    maxT = round (40 / hStep)  -- t = 0..40
    (state1, trace) = model hStep maxT

  -- z vs x
  mapM_ (putStrLn. (\(V3 x _ z) -> show x ++ " " ++ show z)) $ V.toList trace
