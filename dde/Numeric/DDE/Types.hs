{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Numeric.DDE.Types (
    RHS (..)
  , HistorySnapshot (..)
  , Input (..)
  , InputSnapshot (..)
  , Stepper (..)
  ) where

import           Foreign.Storable ( Storable (..) )
import qualified Data.VectorSpace.Free as Free
import qualified Data.Vector.Storable as V


-- | DDE right-hand side.
--
-- Parameter @state@ is and abstraction of a dynamical system's state,
-- i.e. it can be a vector of any length (x(t), y(t), ...).
newtype RHS state = RHS {
  _state
    :: (state, HistorySnapshot state, InputSnapshot) -> state
  }

-- | Input u(t) is one-dimensional
newtype InputSnapshot = Inp { _insnap :: Double }

-- | Vector of input data points
newtype Input = Input { _input :: V.Vector Double }

-- | Contains only the required snapshot of history to make steppers (e.g. Heun) work.
-- There could be several delay variables
newtype HistorySnapshot state = Hist { _histsnap :: state }

-- | DDE stepper (all delays are equal).
--
-- Stepper is a function of the following arguments:
--
-- * Integration step
-- * DDE right-hand side
-- * Current state vector @(x(t), y(t), ...)@
-- * Two subsequent history snapshots
-- * Two subsequent inputs
--
-- The result (step) is a new state vector.
type Stepper = 
       forall state. ( Functor state, Free.VectorSpace (state Double)
                     , Num (Free.Scalar (state Double)) )
     => Free.Scalar (state Double)
     -> RHS (state Double)
     -> state Double
     -> (HistorySnapshot (state Double), HistorySnapshot (state Double))
     -> (Double, Double)
     -> state Double
-- NB: to allow multiple delay times, instead of
-- (HistorySnapshot state, HistorySnapshot state)
-- there should be
-- (HistorySnapshot delaystate, HistorySnapshot delaystate).
-- i.e. a vector of required delayed values (e.g. x(t-tau1), x(t-tau2), y(t-tau3))
