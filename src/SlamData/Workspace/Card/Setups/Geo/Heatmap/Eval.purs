module SlamData.Workspace.Card.Setups.Geo.Heatmap.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Geo.Heatmap.Model
  ) where

import SlamData.Prelude

import Data.List as L

import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Geo.Heatmap.Model (ModelR, Model)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Common.Eval as BCE

import SqlSquared as Sql

eval ∷ ∀ m. BCE.ChartSetupEval ModelR m
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildGeoHeatmap

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.lat # SCC.jcursorPrj # Sql.as "lat"
  , r.lng # SCC.jcursorPrj # Sql.as "lng"
  , r.intensity # SCC.jcursorPrj # Sql.as "intensity"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable
    [ r.lat # SCC.jcursorSql
    , r.lng # SCC.jcursorSql
    ]

buildGeoHeatmap ∷ ModelR → Axes → Port.Port
buildGeoHeatmap m axes =
  Port.GeoChart { build: \leaf records → pure [ ] }
