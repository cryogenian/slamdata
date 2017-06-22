module SlamData.Workspace.Card.Setups.Viz.Component.State where

import SlamData.Prelude

import Data.Map as Map
import Data.Variant (inj)

import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.Setups.Viz.Auxiliary as Aux

type State =
  { vizType ∷ VT.VizType
  , vizTypePickerExpanded ∷ Boolean
  , axes ∷ Maybe Ax.Axes
  , dimMaps ∷ Map.Map VT.VizType T.DimensionMap
  , auxes ∷ Map.Map VT.VizType Aux.State
  }


initialState ∷ State
initialState =
  { vizType: VT.Chart VT.Pie
  , vizTypePickerExpanded: false
  , axes: Nothing
  , dimMaps: Map.fromFoldable $ map (_ × T.emptyDimMap) VT.all
  , auxes: Map.fromFoldable
      $ [ VT.Geo VT.GeoHeatmap × inj Aux._geoHeatmap Aux.initialGeoHeatmap
        , VT.Geo VT.GeoMarker × inj Aux._geoMarker Aux.initialGeoMarker
        , VT.Chart VT.Area × inj Aux._area Aux.initialArea
        , VT.Chart VT.Bar × inj Aux._bar Aux.initialBar
        , VT.Chart VT.Funnel × inj Aux._funnel Aux.initialFunnel
        , VT.Chart VT.Graph × inj Aux._graph Aux.initialGraph
        , VT.Chart VT.Heatmap × inj Aux._heatmap Aux.initialHeatmap
        , VT.Chart VT.Line × inj Aux._line Aux.initialLine
        , VT.Metric × inj Aux._metric Aux.initialMetric
        , VT.Chart VT.PunchCard × inj Aux._punchCard Aux.initialPunchCard
        , VT.Chart VT.Scatter × inj Aux._scatter Aux.initialScatter
        ]
  }
