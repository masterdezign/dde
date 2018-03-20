{- |
   = Example DDE models

-}
module Numeric.DDE.Model (
  BandpassFiltering (..)
  , RC (..)
  , MackeyGlass (..)
  , mackeyGlassRhs
  , bandpassRhs
  ) where

import           Control.Lens
import           Linear ( R1 (..)
                        , R2 (..) )
import qualified Linear.V1
import qualified Linear.V2
import qualified Data.Vector.Storable as V

import           Numeric.DDE.Types


-- | Mackey-Glass model (no external input) parameters
data MackeyGlass =
         MackeyGlass { _beta :: Double
                       , _gamma :: Double
                       }

--  | Bandpass-filtered two-dimensional nonlinear system with
--  external input @u(t)@.
--
-- \[
-- \begin{aligned}
--   \tau dx(t)/dt &= -x - y / \theta + fnl[x(t - \tau_D) + \rho u(t)] \\
--          dy(y)/dt &= x
-- \end{aligned}
-- \]
data RC =
         RC { _fnl :: Double -> Double  -- ^ Nonlinear transformation
            , _rho :: Double  -- ^ External forcing weight
            , _filt :: BandpassFiltering
            }

-- | Bandpass filter (linear) parameters
data BandpassFiltering = BandpassFiltering
  { _tau :: Double  -- ^ System response time, s (epsilon = \tau / \tau_D)
  , _theta :: Double  -- ^ Integration time, s  (delta = \tau_D / theta)
  } deriving Show

-- | Mackey-Glass model with no external forcing
mackeyGlassRhs :: MackeyGlass -> RHS (Linear.V1.V1 Double)
mackeyGlassRhs MackeyGlass { _beta = beta, _gamma = gamma } = RHS _f
  where
    _f (xs, (Hist hs), _) = Linear.V1.V1 x'
      where
        x' = beta * x_tau / (1 + x_tau^(10::Int)) - gamma * x
        x = xs ^._x
        x_tau = hs ^._x

-- Ikeda-like model with an integral term y(t) and external input
bandpassRhs :: RC -> RHS (Linear.V2.V2 Double)
bandpassRhs RC { _fnl = _fnl,
                 _rho = _rho,
                 _filt = BandpassFiltering { _tau = tau, _theta = theta }
               } = RHS _f
 where
   _f (xs, (Hist hs), (Inp u)) = Linear.V2.V2 x' y'
     where
       x' = (-x - (recip theta) * y + _fnl (x_tau + _rho * u)) / tau
       y' = x  -- Integral term

       x = xs ^._x
       y = xs ^._y
       x_tau = hs ^._x  -- Delay term
