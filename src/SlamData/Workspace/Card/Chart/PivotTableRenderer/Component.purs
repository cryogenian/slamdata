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

module SlamData.Workspace.Card.Chart.PivotTableRenderer.Component where

import SlamData.Prelude
import Data.Argonaut as J
import Data.Array as Array
import Data.Foldable as F
import Data.List as List
import Data.Map as Map
import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model (PivotTableR, Column(..))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Pivot as Pivot

type State =
  { records ∷ Array J.Json
  , options ∷ PivotTableR
  }

initialState ∷ State
initialState =
  { records: []
  , options: { dimensions: [], columns: [] }
  }

data Query a = Update (Array J.Json) PivotTableR a

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render st =
  HH.div
    [ HP.classes [ HH.className "sd-pivot-table" ] ]
    [ case Array.length st.options.dimensions, Array.length st.options.columns of
        _, 0 → HH.text ""
        0, _ → renderFlat st.options.columns st.records
        _, _ → renderTable st.options.dimensions st.options.columns st.records
    ]
  where
  renderFlat cols rows =
    HH.table_ $
      [ HH.tr_ (map (\(Column { value }) → HH.th_ [ HH.text (show value) ]) cols)
      ] <> map HH.tr_ (renderLeaves cols rows)

  renderTable dims cols rows =
    let
      dims' = List.fromFoldable (map J.cursorGet dims)
      pivot = Pivot.classify dims' rows in
    HH.table_ $
      [ HH.tr_ $
          [ HH.td [ HP.colSpan (Array.length dims) ] []
          ] <> map (\(Column { value }) → HH.th_ [ HH.text (show value) ]) cols
      ] <> (renderRows cols pivot)

  renderRows cols =
    map HH.tr_ <<< Pivot.fold (renderLeaves cols) renderHeadings

  renderLeaves cols rows = snd (foldl go (true × []) rows)
    where
    allAg = F.all (\(Column { valueAggregation }) → isJust valueAggregation) cols

    go (firstRow × acc) row | allAg && not firstRow = false × acc
    go (firstRow × acc) row =
      let
        cs = flip foldMap cols case _ of
          Column { value, valueAggregation: Just ag } | firstRow →
            [ HH.td_ [ HH.text (show (Ag.runAggregation ag (jsonNumbers value rows))) ] ]
          Column { value, valueAggregation: Just _ } →
            [ HH.td_ [ HH.text "" ] ]
          Column { value } →
            [ HH.td_ [ HH.text (maybe "" renderJson (J.cursorGet value row)) ] ]
      in
        false × (acc <> pure cs)

  jsonNumbers cursor =
    Array.mapMaybe (J.cursorGet cursor >=> J.foldJsonNumber Nothing Just)

  renderColumn json (Column { value, valueAggregation }) =
    case J.cursorGet value json of
      Nothing → HH.td_ [ HH.text "" ]
      Just a  → HH.td_ [ HH.text (renderJson a) ]

  renderHeadings m =
    foldMap renderHeading (Map.toList m)

  renderHeading (k × rs) =
    case Array.uncons rs of
      Just { head, tail } →
        Array.cons
          (Array.cons
            (HH.th [ HP.rowSpan (Array.length rs) ] [ HH.text (renderJson k) ])
            head)
          tail
      Nothing →
        []

  renderJson =
    J.foldJson show show show id show show

eval ∷ Query ~> DSL
eval (Update records options next) = do
  H.set { records, options }
  pure next
