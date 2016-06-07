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

module SlamData.Workspace.Card.Viz.Component.State
  ( State
  , initialState
  , _chartType
  , _availableChartTypes
  , _loading
  , _sample
  , _records
  , _needToUpdate
  , _axisLabelAngle
  , _axisLabelFontSize
  , _levelOfDetails
  , StateP
  , fromModel
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor, JArray)
import Data.Lens (LensP, lens)
import Data.Map as M
import Data.Set as Set

import Halogen (ParentState)

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Chart.Axis (Axis)
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.Viz.Component.Query (Query)
import SlamData.Workspace.Card.Viz.Form.Component as Form
import SlamData.Workspace.Card.Viz.Model (Model)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))


type State =
  { chartType ∷ ChartType
  , availableChartTypes ∷ Set.Set ChartType
  , sample ∷ M.Map JCursor Axis
  , loading ∷ Boolean
  , records ∷ JArray
  , needToUpdate ∷ Boolean
  , axisLabelFontSize ∷ Int
  , axisLabelAngle ∷ Int
  , levelOfDetails ∷ LevelOfDetails
  }

initialState ∷ State
initialState =
  { chartType: Pie
  , availableChartTypes: Set.empty
  , loading: true
  , sample: M.empty
  , records: []
  , needToUpdate: true
  , axisLabelFontSize: 12
  , axisLabelAngle: 30
  , levelOfDetails: High
  }

_chartType ∷ forall a r. LensP {chartType ∷ a |r} a
_chartType = lens _.chartType _{chartType = _}

_availableChartTypes ∷ forall a r. LensP {availableChartTypes ∷ a |r} a
_availableChartTypes = lens _.availableChartTypes _{availableChartTypes = _}

_loading ∷ forall a r. LensP {loading ∷ a | r} a
_loading = lens _.loading _{loading = _}

_sample ∷ forall a r. LensP {sample ∷ a | r} a
_sample = lens _.sample _{sample = _}

_records ∷ forall a r. LensP {records ∷ a | r} a
_records = lens _.records _{records = _}

_needToUpdate ∷ forall a r. LensP {needToUpdate ∷ a | r} a
_needToUpdate = lens _.needToUpdate _{needToUpdate = _}

_axisLabelFontSize ∷ forall a r. LensP {axisLabelFontSize ∷ a | r} a
_axisLabelFontSize = lens _.axisLabelFontSize _{axisLabelFontSize = _}

_axisLabelAngle ∷ forall a r. LensP {axisLabelAngle ∷ a | r} a
_axisLabelAngle = lens _.axisLabelAngle _{axisLabelAngle = _}

_levelOfDetails ∷ ∀ a r. LensP {levelOfDetails ∷ a|r} a
_levelOfDetails = lens (_.levelOfDetails) (_{levelOfDetails = _})


type StateP =
  ParentState
    State
    Form.StateP
    (Coproduct CardEvalQuery Query)
    Form.QueryP
    Slam ChartType

fromModel ∷ Model → State
fromModel m =
  initialState
    { chartType = m.chartType
    , axisLabelFontSize = m.axisLabelFontSize
    , axisLabelAngle = m.axisLabelAngle
    }
