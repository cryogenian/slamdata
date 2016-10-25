{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Ace.Component.State
  ( StateP
  , State
  , Status(..)
  , initialState
  , _levelOfDetails
  , _status
  , isNew
  , isLoading
  , isReady
  ) where

import SlamData.Prelude

import Ace.Halogen.Component (AceQuery, AceState)

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.EventLoop (Breaker)

import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Ace.Component.Query (Query)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

data Status
  = New
  | Loading
  | Ready

type State =
  { levelOfDetails ∷ LevelOfDetails
  , status ∷ Status
  , dirty ∷ Boolean
  , trigger ∷ Maybe (AVar Unit)
  , breaker ∷ Maybe (Breaker Unit)
  }

initialState ∷ State
initialState =
  { levelOfDetails: High
  , status: New
  , dirty: false
  , trigger: Nothing
  , breaker: Nothing
  }

_levelOfDetails ∷ ∀ a r. LensP {levelOfDetails ∷ a |r} a
_levelOfDetails = lens (_.levelOfDetails) (_{levelOfDetails = _})

_status ∷ ∀ a r. LensP {status ∷ a|r} a
_status = lens (_.status) (_{status = _})

isNew ∷ Status → Boolean
isNew New = true
isNew _   = false

isLoading ∷ Status → Boolean
isLoading Loading = true
isLoading _       = false

isReady ∷ Status → Boolean
isReady Ready = true
isReady _     = false

type StateP = ParentState State AceState (CardEvalQuery ⨁ Query) AceQuery Slam Unit
