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

module SlamData.Workspace.Card.BuildChart.PivotTable.Component where

import SlamData.Prelude

import Data.Argonaut (cursorGet, foldJson)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.List as List

import CSS as C
import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.CSS.Indexed as HC

import SlamData.Monad (Slam)
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Card.BuildChart.PivotTable.Component.ChildSlot as PCS
import SlamData.Workspace.Card.BuildChart.PivotTable.Component.Query (Query(..), QueryC, QueryP)
import SlamData.Workspace.Card.BuildChart.PivotTable.Component.State (State, StateP, modelFromState, initialState)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model (Column(..))
import SlamData.Workspace.Card.BuildChart.PivotTable.Pivot as Pivot
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Chart.Axis (analyzeJArray)
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = H.ParentDSL State PCS.ChildState QueryC PCS.ChildQuery Slam PCS.ChildSlot

type HTML = H.ParentHTML PCS.ChildState QueryC PCS.ChildQuery Slam PCS.ChildSlot

pivotTableBuilderComponent ∷ CC.CardComponent
pivotTableBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.PivotTable
  , component: H.parentComponent
      { render
      , eval: coproduct evalCard evalOptions
      , peek: Just peek
      }
  , initialState: H.parentState initialState
  , _State: CC._BuildPivotTableState
  , _Query: CC.makeQueryPrism' CC._BuildPivotTableQuery
  }

render ∷ State → HTML
render st =
  if st.levelOfDetails ≡ High
    then renderHighLOD st
    else renderLowLOD (CT.darkCardGlyph (CT.ChartOptions CHT.PivotTable)) left st.levelOfDetails

renderHighLOD ∷ State → HTML
renderHighLOD st =
  HH.div
    [ HP.classes [ HH.className "sd-pivot-options" ] ]
    [ HH.div
        [ HP.classes [ HH.className "sd-pivot-options-dims" ] ]
        renderedDimensions
    , HH.div
        [ HP.classes [ HH.className "sd-pivot-options-cols" ] ]
        renderedColumns
    ]
  where
  renderedDimensions =
    let
      dims = Array.fromFoldable (Map.toList st.dimensions)
      len  = Array.length dims + 1
      size = 100.0 / toNumber len
    in
      map (renderDimension size) dims <>
      [ HH.div
          [ HP.classes [ HH.className "sd-pivot-options-dim" ]
          , HC.style (C.height (C.pct size))
          ]
          [ HH.button
              [ HP.classes [ HH.className "sd-pivot-options-plus" ] ]
              []
          ]
      ]
  
  renderDimension size (slot × dim) =
    HH.div
      [ HP.classes [ HH.className "sd-pivot-options-dim" ]
      , HC.style (C.height (C.pct size))
      ]
      [ HH.text (show dim)
      ]

  renderedColumns =
    let
      cols = Array.fromFoldable (Map.toList st.columns)
      len  = Array.length cols + 1
      size = 100.0 / toNumber len
    in
      map (renderColumn size) cols <>
      [ HH.div
          [ HP.classes [ HH.className "sd-pivot-options-col" ]
          , HC.style (C.width (C.pct size))
          ]
          [ HH.div
              [ HP.classes [ HH.className "sd-pivot-options-col-value" ] ]
              [ HH.button
                  [ HP.classes [ HH.className "sd-pivot-options-plus" ] ]
                  []
              ]
          , HH.div
              [ HP.classes [ HH.className "sd-pivot-options-col-aggregation" ] ]
              []
          ]
      ]
  
  renderColumn size (slot × (Column col)) =
    HH.div
      [ HP.classes [ HH.className "sd-pivot-options-col" ]
      , HC.style (C.height (C.pct size))
      ]
      [ HH.div
          [ HP.classes [ HH.className "sd-pivot-options-col-value" ] ]
          [ HH.text (show col.value) ]
      , HH.div
          [ HP.classes [ HH.className "sd-pivot-options-col-aggregation" ] ]
          []
      ]

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.EvalCard info _ next →
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.SetDimensions _ next →
    pure next
  CC.Save k →
    map (k ∘ Card.BuildPivotTable ∘ modelFromState) H.get
  CC.Load card next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

evalOptions ∷ Query ~> DSL
evalOptions = case _ of
  AddDimension next →
    pure next
  AddColumn next →
    pure next

peek ∷ ∀ a. H.ChildF PCS.ChildSlot PCS.ChildQuery a → DSL Unit
peek _ = pure unit
