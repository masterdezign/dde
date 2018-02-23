{- |
   = Example DDE models

   These example models are defined by `rhs` function.
   Adding a new example model requires adjusting `rhs` and  `Par`.
   Switching between models is performed by changing `Par`.
-}
module Numeric.DDE.Model (
    Par (..)
  , BandpassFiltering (..)
  , rhs
  ) where

import qualified Data.Vector.Storable as V

import           Numeric.DDE.Types


-- | Model parameters
data Par =
         -- | Mackey-Glass model (no external input)
         MackeyGlass { _beta :: Double
                       , _gamma :: Double
                       }
         |

         --  | Bandpass-filtered two-dimensional nonlinear system with
         --  external input @u(t)@.
         --
         --     \tau * dx(t)/dt = -x - y / theta + fnl[x(t - \tau_D) + \rho*u(t)]
         --            dy(y)/dt = x
         RC { _fnl :: Double -> Double  -- ^ Nonlinear transformation
            , _rho :: Double  -- ^ External forcing weight
            , _filt :: BandpassFiltering
            }

-- | Bandpass filter (linear) parameters
data BandpassFiltering = BandpassFiltering
  { _tau :: Double  -- ^ System response time, s (epsilon = \tau / \tau_D)
  , _theta :: Double  -- ^ Integration time, s  (delta = \tau_D / theta)
  } deriving Show

-- | DDE right-hand sides for example models
rhs :: Par -> RHS

-- Mackey-Glass model with no external forcing
rhs MackeyGlass { _beta = beta, _gamma = gamma }
    ((State xs), (Hist hs), _)
      = State (V.singleton x')
        where x' = beta * x_tau / (1 + x_tau^(10::Int)) - gamma * x
              x = xs V.! 0
              x_tau = hs V.! 0

-- Ikeda-like model with an integral term y(t) and external input
rhs RC { _fnl = _fnl,
         _rho = _rho,
         _filt = BandpassFiltering { _tau = _tau, _theta = _theta }
       }
       ((State xs), (Hist hs), (Inp u)) = State $ V.fromList [x', y']
         where
           x' = (-x - (recip _theta) * y + _fnl (x_tau + _rho * u)) / _tau
           y' = x  -- Integral term

           x = xs V.! 0
           y = xs V.! 1
           x_tau = hs V.! 0  -- Delay term
