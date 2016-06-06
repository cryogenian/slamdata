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

module SlamData.Workspace.Card.Chart.Component.State where

import Prelude (Unit)

import Data.Lens (lens, LensP)

import Halogen (ParentState)
import Halogen.ECharts (EChartsState, EChartsQuery)

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Effects (Slam)

type State =
  { width ∷ Int
  , height ∷ Int
  , levelOfDetails ∷ LevelOfDetails
  }

_levelOfDetails ∷ ∀ a r. LensP {levelOfDetails ∷ a|r} a
_levelOfDetails = lens (_.levelOfDetails) (_{levelOfDetails = _})

type StateP =
  ParentState State EChartsState CardEvalQuery EChartsQuery Slam Unit

initialState :: State
initialState =
  { width: 600
  , height: 400
  , levelOfDetails: High
  }
