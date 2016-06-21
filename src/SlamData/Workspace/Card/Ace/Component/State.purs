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
  , initialState
  , _levelOfDetails
  , _isNew
  ) where

import SlamData.Prelude

import Ace.Halogen.Component (AceQuery, AceState)

import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Effects (Slam)

type State =
  { levelOfDetails ∷ LevelOfDetails
  , isNew ∷ Boolean
  }

initialState ∷ State
initialState =
  { levelOfDetails: High
  , isNew: true
  }

_levelOfDetails ∷ ∀ a r. LensP {levelOfDetails ∷ a |r} a
_levelOfDetails = lens (_.levelOfDetails) (_{levelOfDetails = _})

_isNew ∷ ∀ a r. LensP {isNew ∷ a|r} a
_isNew = lens (_.isNew) (_{isNew = _})

type StateP = ParentState State AceState CardEvalQuery AceQuery Slam Unit
