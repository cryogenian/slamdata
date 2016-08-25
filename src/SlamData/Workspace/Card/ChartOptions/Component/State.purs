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
  , StateP
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
  , _funnelOrder
  , _funnelAlign
  , _minColorVal
  , _maxColorVal
  , _colorScheme
  , _colorReversed
  , fromModel
  ) where

import SlamData.Prelude

import Data.Lens (LensP, lens)
import Data.Set as Set

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))
import SlamData.Workspace.Card.ChartOptions.Component.Query (QueryC)
import SlamData.Workspace.Card.ChartOptions.Model (Model)
import SlamData.Workspace.Card.Chart.Config (ChartConfig(..))
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import SlamData.Workspace.Card.ChartOptions.Component.Install (ChildQuery, ChildSlot, ChildState)

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
  , funnelOrder :: String
  , funnelAlign :: String
  , minColorVal :: Number
  , maxColorVal :: Number
  , colorScheme :: String
  , colorReversed :: Boolean
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
  , funnelOrder: "descending"
  , funnelAlign: "center"
  , minColorVal: 0.0
  , maxColorVal: 1.0
  , colorScheme: "diverging: red-blue"
  , colorReversed: false
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

_funnelOrder ∷ ∀ a r. LensP {funnelOrder ∷ a | r} a
_funnelOrder = lens _.funnelOrder _{funnelOrder = _}

_funnelAlign ∷ ∀ a r. LensP {funnelAlign ∷ a | r} a
_funnelAlign = lens _.funnelAlign _{funnelAlign = _}

_minColorVal ∷ ∀ a r. LensP {minColorVal ∷ a | r} a
_minColorVal = lens _.minColorVal _{minColorVal = _}

_maxColorVal ∷ ∀ a r. LensP {maxColorVal ∷ a | r} a
_maxColorVal = lens _.maxColorVal _{maxColorVal = _}

_colorScheme ∷ ∀ a r. LensP {colorScheme ∷ a | r} a
_colorScheme = lens _.colorScheme _{colorScheme = _}

_colorReversed ∷ ∀ a r. LensP {colorReversed ∷ a | r} a
_colorReversed = lens _.colorReversed _{colorReversed = _}

fromModel ∷ Model → State
fromModel (Just (Legacy {options})) =
  initialState
    { chartType = options.chartType
    , axisLabelFontSize = options.axisLabelFontSize
    , axisLabelAngle = options.axisLabelAngle
    , areaStacked = options.areaStacked
    , smooth = options.smooth
    , bubbleMinSize = options.bubbleMinSize
    , bubbleMaxSize = options.bubbleMaxSize
    , funnelOrder = options.funnelOrder
    , funnelAlign = options.funnelAlign
    , minColorVal = options.minColorVal
    , maxColorVal = options.maxColorVal
    , colorScheme = options.colorScheme
    , colorReversed = options.colorReversed
    }
fromModel _ = initialState


type StateP = ParentState State ChildState QueryC ChildQuery Slam ChildSlot
