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

module SlamData.Workspace.Card.Setups.Chart.Area.Component
  ( areaBuilderComponent
  ) where

import SlamData.Prelude


import Data.Argonaut as J
import Data.Lens (Traversal', _Just, (.~), (^?), (%~), (^.), (?~))
import Data.List as L
import Data.Set as Set
import Data.StrMap as SM

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.ChildPath (ChildPath, cp1)

import SlamData.Render.Common (row)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.Chart.Area.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Area.Component.State as ST
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Setups.Dimension as D

import SlamData.Workspace.Card.Setups.Common.Component as SC
import SlamData.Workspace.Card.Setups.Common.State as C

type DSL = CC.InnerCardParentDSL ST.State Q.Query ChildQuery ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query ChildQuery ChildSlot

type ChildSlot = Unit ⊹ Void
type ChildQuery = SC.Query ⨁ Const Void

type Path a b = ChildPath a ChildQuery b ChildSlot

cpDims ∷ Path SC.Query Unit
cpDims = cp1

package ∷ SC.Package
package =
  { allFields: L.fromFoldable [ C._dimension, C._value, C._series ]
  , cursorMap
  , load
  , save
  }
  where
  cursorMap ∷ SC.State → SM.StrMap (Set.Set J.JCursor)
  cursorMap st =
    let
      _projection ∷ Traversal' (Maybe D.LabeledJCursor) J.JCursor
      _projection = _Just ∘ D._value ∘ D._projection

      axes = st ^. C._axes

      dimension =
        axes.category
        ⊕ axes.time
        ⊕ axes.value
        ⊕ axes.date
        ⊕ axes.datetime

      value =
        C.mbDelete (st ^? C._dimMap ∘ C.unpack C._dimension ∘ _projection)
        $ axes.value

      series =
        C.mbDelete (st ^? C._dimMap ∘ C.unpack C._dimension ∘ _projection)
        $ C.mbDelete (st ^? C._dimMap ∘ C.unpack C._value ∘ _projection)
        $ C.ifSelected (st ^? C._dimMap ∘ C.unpack C._dimension ∘ _projection)
        $ axes.value
        ⊕ axes.time
        ⊕ axes.datetime
        ⊕ axes.category

    in
     SM.empty
       # ( C.unpack C._dimension ?~ dimension )
       ∘ ( C.unpack C._value ?~ value )
       ∘ ( C.unpack C._series ?~ series)

  fldCursors ∷ C.Projection → SC.State → Set.Set J.JCursor
  fldCursors fld st = fromMaybe Set.empty $ cursorMap st ^. C.unpack fld

  cursors ∷ SC.State → L.List J.JCursor
  cursors st = case st.selected of
    Just (Left lns) → L.fromFoldable $ fldCursors lns st
    _ → L.Nil

  disabled ∷ C.Projection → SC.State → Boolean
  disabled fld st = Set.isEmpty $ fldCursors fld st

  save ∷ SC.State → M.AnyCardModel
  save st =
    M.BuildArea
    $ { dimension: _
      , value: _
      , series: st ^. C._dimMap ∘ C.unpack C._series
      , axisLabelAngle: 0.0
      , isSmooth: false
      , isStacked: false
      }
    <$> (st ^. C._dimMap ∘ C.unpack C._dimension)
    <*> (st ^. C._dimMap ∘ C.unpack C._value)

  load ∷ M.AnyCardModel → SC.State → SC.State
  load = case _ of
    M.BuildArea (Just m) →
      ( C._dimMap ∘ C.unpack C._dimension .~ Just m.dimension )
      ∘ ( C._dimMap ∘ C.unpack C._value .~ Just m.value )
      ∘ ( C._dimMap ∘ C.unpack C._series .~ m.series )
    _ → id


areaBuilderComponent ∷ CC.CardOptions → CC.CardComponent
areaBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Area) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }
render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ]
    ]
    [ HH.slot' cpDims unit (SC.component package) unit $ HE.input \l → right ∘ Q.HandleDims l
    , HH.hr_
    , row [ renderIsStacked state, renderIsSmooth state ]
    , row [ renderAxisLabelAngle state ]
    ]

renderAxisLabelAngle ∷ ST.State → HTML
renderAxisLabelAngle state =
  HH.div
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label angle" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.axisLabelAngle
        , ARIA.label "Axis label angle"
        , HE.onValueChange $ HE.input \l → right ∘ Q.SetAxisLabelAngle l
        ]
    ]

renderIsStacked ∷ ST.State → HTML
renderIsStacked state =
  HH.div
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Stacked" ]
    , HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked state.isStacked
        , ARIA.label "Stacked"
        , HE.onChecked $ HE.input_ $ right ∘ Q.ToggleStacked
        ]

    ]

renderIsSmooth ∷ ST.State → HTML
renderIsSmooth state =
  HH.div
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Smooth" ]
    , HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked state.isSmooth
        , ARIA.label "Smooth"
        , HE.onChecked $ HE.input_ $ right ∘ Q.ToggleSmooth
        ]
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    m ← H.query' cpDims unit $ H.request SC.Save
    let
      model = case m of
        Just (M.BuildArea (Just r)) →
          M.BuildArea $ Just r { axisLabelAngle = st.axisLabelAngle
                               , isStacked = st.isStacked
                               , isSmooth = st.isSmooth
                               }
        _ → M.BuildArea Nothing

    pure $ k model
  CC.Load m next → do
    H.query' cpDims unit $ H.action $ SC.Load m
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Axes) \axes → do
      H.query' cpDims unit $ H.action $ SC.SetAxes axes
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

raiseUpdate ∷ DSL Unit
raiseUpdate =
  H.raise CC.modelUpdate

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.SetAxisLabelAngle str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify $ ST._axisLabelAngle .~ fl
      raiseUpdate
    pure next
  Q.ToggleSmooth next → do
    H.modify $ ST._isSmooth %~ not
    raiseUpdate
    pure next
  Q.ToggleStacked next → do
    H.modify $ ST._isStacked %~ not
    raiseUpdate
    pure next
  Q.HandleDims q next → do
    case q of
      SC.Update → raiseUpdate
    pure next
