import           Linear ( V2 (..) )
import qualified Data.Vector.Storable as V
import qualified Numeric.DDE as DDE

rhs phi0 = DDE.RHS derivative
  where
    derivative ((V2 x y), (DDE.Hist histSnapshots), _) = V2 x' y'
      where
        -- DDE Eq. (4) from arXiv:1712.03283
        x' = (-x - delta * y + (1 - gamma) * f x_tau1 + gamma * f x_tau2) / epsilon
        y' = x

        f = airy phi0

        -- Delay terms where tau2 / tau1 = 100
        (V2 x_tau1 _):(V2 x_tau2 _):_ = histSnapshots

        -- Constants
        epsilon = 0.01
        gamma = 0.5
        delta = 0.009

airy phi0 x = beta / (1 + m * (sin (x + phi0))^2)
  where
    m = 50
    beta = 1.6

model phi0 hStep len1 len2 totalIter = (state1, V.map (\(V2 x y) -> x) trace)
  where
    -- Initial conditions:
    -- dynamical state and delay history.
    state0 = V2 0.0 0.0
    hist0 = V.fromList $ map (\n -> let x = sin(2 * pi * fromIntegral n / 1000)
                                    in V2 x 0.0) [1..len2]

    -- Input is ignored in `rhs`
    inp = DDE.Input $ V.replicate (totalIter + 1) 0

    delaysInSamples = [len1, len2]

    (state1, trace) = DDE.integ DDE.rk4 state0 hist0 delaysInSamples hStep (rhs phi0) inp

-- Control parameter
phi0 = -0.45

main = do
  let hStep = 0.0005  -- Integration step
      tauD1 = 1.0  -- Short delay time
      len1 = round $ tauD1 / hStep  -- Samples per short delay
      len2 = 100 * len1  -- Long delay (in samples)
      delays = 10  -- 10 long delays
      total = delays * len2

  let (state1, trace) = model phi0 hStep len1 len2 total

  mapM_ print $ V.toList trace

