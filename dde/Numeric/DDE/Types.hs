{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Numeric.DDE.Types (
    RHS (..)
  , HistorySnapshots (..)
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
    :: (state, HistorySnapshots state, InputSnapshot) -> state
  }

-- | Input u(t) is one-dimensional
newtype InputSnapshot = Inp { _insnap :: Double }

-- | Vector of input data points
newtype Input = Input { _input :: V.Vector Double }

-- | Contains state snapshots corresponding to each required delay length
newtype HistorySnapshots state = Hist { _histsnaps :: [state] }

-- | DDE stepper
--
-- Stepper is a function of the following arguments:
--
-- * Integration step
-- * DDE right-hand side
-- * Current state vector @(x(t), y(t), ...)@
-- * Two subsequent history snapshot lists
-- * Two subsequent inputs
--
-- The result (step) is a new state vector.
type Stepper =
       forall state. ( Functor state, Free.VectorSpace (state Double)
                     , Num (Free.Scalar (state Double)) )
     => Free.Scalar (state Double)
     -> RHS (state Double)
     -> state Double
     -> (HistorySnapshots (state Double), HistorySnapshots (state Double))
     -> (Double, Double)
     -> state Double
