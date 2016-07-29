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

module SlamData.Workspace.Card.ChartOptions.Component.State
  ( State
  , initialState
  , _chartType
  , _availableChartTypes
  , _axes
  , _axisLabelAngle
  , _axisLabelFontSize
  , _levelOfDetails
  , _areaStacked
  , _smooth
  , _bubbleMinSize
  , _bubbleMaxSize
  , StateP
  , fromModel
  ) where

import SlamData.Prelude

import Data.Lens (LensP, lens)
import Data.Set as Set

import Halogen (ParentState)

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.ChartOptions.Component.Query (Query)
import SlamData.Workspace.Card.ChartOptions.Form.Component as Form
import SlamData.Workspace.Card.ChartOptions.Model (Model)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))


type State =
  { chartType ∷ ChartType
  , availableChartTypes ∷ Set.Set ChartType
  , axes ∷ Axes
  , axisLabelFontSize ∷ Int
  , axisLabelAngle ∷ Int
  , levelOfDetails ∷ LevelOfDetails
  , areaStacked :: Boolean
  , smooth :: Boolean
  , bubbleMinSize :: Number
  , bubbleMaxSize :: Number
  }

initialState ∷ State
initialState =
  { chartType: Pie
  , availableChartTypes: Set.empty
  , axes: {value: [], category: [], time: []}
  , axisLabelFontSize: 12
  , axisLabelAngle: 0
  , levelOfDetails: High
  , areaStacked: false
  , smooth: false
  , bubbleMinSize: 1.0
  , bubbleMaxSize: 50.0
  }

_chartType ∷ ∀ a r. LensP {chartType ∷ a |r} a
_chartType = lens _.chartType _{chartType = _}

_availableChartTypes ∷ ∀ a r. LensP {availableChartTypes ∷ a |r} a
_availableChartTypes = lens _.availableChartTypes _{availableChartTypes = _}

_axes ∷ ∀ a r. LensP {axes ∷ a|r} a
_axes = lens _.axes _{axes =_}

_axisLabelFontSize ∷ ∀ a r. LensP {axisLabelFontSize ∷ a | r} a
_axisLabelFontSize = lens _.axisLabelFontSize _{axisLabelFontSize = _}

_axisLabelAngle ∷ ∀ a r. LensP {axisLabelAngle ∷ a | r} a
_axisLabelAngle = lens _.axisLabelAngle _{axisLabelAngle = _}

_levelOfDetails ∷ ∀ a r. LensP {levelOfDetails ∷ a|r} a
_levelOfDetails = lens (_.levelOfDetails) (_{levelOfDetails = _})

_smooth ∷ ∀ a r. LensP {smooth ∷ a | r} a
_smooth = lens _.smooth _{smooth = _}

_areaStacked ∷ ∀ a r. LensP {areaStacked ∷ a | r} a
_areaStacked = lens _.areaStacked _{areaStacked = _}

_bubbleMinSize ∷ ∀ a r. LensP {bubbleMinSize ∷ a | r} a
_bubbleMinSize = lens _.bubbleMinSize _{bubbleMinSize = _}

_bubbleMaxSize ∷ ∀ a r. LensP {bubbleMaxSize ∷ a | r} a
_bubbleMaxSize = lens _.bubbleMaxSize _{bubbleMaxSize = _}

type StateP =
  ParentState
    State
    Form.StateP
    (Coproduct CardEvalQuery Query)
    Form.QueryP
    Slam ChartType

fromModel ∷ Model → State
fromModel { options } =
  initialState
    { chartType = options.chartType
    , axisLabelFontSize = options.axisLabelFontSize
    , axisLabelAngle = options.axisLabelAngle
    , areaStacked = options.areaStacked
    , smooth = options.smooth
    , bubbleMinSize = options.bubbleMinSize
    , bubbleMaxSize = options.bubbleMaxSize
    }
