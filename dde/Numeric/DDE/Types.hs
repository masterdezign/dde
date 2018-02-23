module Numeric.DDE.Types (
    RHS
  , HistorySnapshot (..)
  , Input (..)
  , InputSnapshot (..)
  , State (..)
  , Stepper1 (..)
  ) where

import qualified Data.Vector.Storable as V


-- | DDE right-hand side
type RHS = (State, HistorySnapshot, InputSnapshot) -> State

-- | State of a dynamical system, it can be a vector of any length (x(t), y(t), ...).
newtype State = State { _state :: V.Vector Double }

-- | Input u(t) is one-dimensional
newtype InputSnapshot = Inp { _insnap :: Double }

-- | Vector of input data points
newtype Input = Input { _input :: V.Vector Double }

-- | Contains only the required snapshot of history to make steppers (e.g. Heun) work.
-- There could be several delay variables
newtype HistorySnapshot = Hist { _histsnap :: V.Vector Double }

-- | Stepper for DDEs with a single delay
--
-- >>> _stepper stepSize rhs xyState xTau1_2 u1_2
newtype Stepper1 = Stepper1 {
  _stepper
    ::  Double
    -> RHS
    -> State
    -> (Double, Double)
    -> (Double, Double)
    -> State
  }
