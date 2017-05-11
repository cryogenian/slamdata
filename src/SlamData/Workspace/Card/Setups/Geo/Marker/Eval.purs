module SlamData.Workspace.Card.Setups.Geo.Marker.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Geo.Marker.Model
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.List as L

import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Geo.Marker.Model (ModelR, Model)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE

import SqlSquared as Sql

import Utils.Array (enumerate)

eval ∷ ∀ m. BCE.ChartSetupEval ModelR m
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildMarker

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  $ [ r.lat # SCC.jcursorPrj # Sql.as "lat"
    , r.lng # SCC.jcursorPrj # Sql.as "lng"
    , r.series # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "series"
    , sizeField
    ]
  ⊕ ( map mkProjection $ enumerate r.dims )
  where
  sizeField = case r.size of
    Nothing → SCC.nullPrj # Sql.as "size"
    Just sz → sz # SCC.jcursorPrj # Sql.as "size" # SCC.applyTransform sz
  mkProjection (ix × field) =
    field # SCC.jcursorPrj # Sql.as ("measure" ⊕ show ix) # SCC.applyTransform field

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ Just $ SCC.jcursorSql r.lat
    , Just $ SCC.jcursorSql r.lng
    , map SCC.jcursorSql r.series
    ]

buildMarker ∷ ModelR → Axes → Port.Port
buildMarker r _ =
  Port.GeoChart { build: \leaf records → pure [ ] }
