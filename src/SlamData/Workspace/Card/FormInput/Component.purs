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

module SlamData.Workspace.Card.FormInput.Component (formInputComponent) where

import SlamData.Prelude

import Data.Int (floor)
import Data.Lens ((^?))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Chart.MetricRenderer.Component as Metric
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State (_AutoSelect)
import SlamData.Workspace.Card.FormInput.Component.ChildSlot as CS
import SlamData.Workspace.Card.FormInput.Component.Query (Query(..))
import SlamData.Workspace.Card.FormInput.Component.State (State, initialState)
import SlamData.Workspace.Card.FormInput.LabeledRenderer.Component as Labeled
import SlamData.Workspace.Card.FormInput.Model as M
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Component as TextLike
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port (Port(..))
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardParentDSL State Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Query CS.ChildQuery CS.ChildSlot

formInputComponent ∷ CC.CardOptions → CC.CardComponent
formInputComponent =
  CC.makeCardComponent CT.FormInput $ H.parentComponent
    { render
    , eval: evalCard ⨁ evalComponent
    , initialState: const initialState
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.div_
    $ flip foldMap state.formInputType \fit →
        if FIT.isLabeled fit
          then labeled
          else if FIT.isTextLike fit
                 then textLike
                 else metric
  where
  textLike = [ HH.slot' CS.cpTextLike unit TextLike.comp unit $ HE.input_ $ right ∘ RaiseUpdate ]
  labeled = [ HH.slot' CS.cpLabeled unit Labeled.comp unit $ HE.input_ $ right ∘ RaiseUpdate ]
  metric = [ HH.slot' CS.cpMetric unit Metric.comp state.dimensions absurd ]


evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    mbTextLike ← H.query' CS.cpTextLike unit $ H.request TextLike.Save
    mbLabeled ← H.query' CS.cpLabeled unit $ H.request Labeled.Save
    let
      mbModel =
        Card.FormInput
          $ fromMaybe M.Static
          $ map M.TextLike mbTextLike
          <|> map M.Labeled mbLabeled
    pure $ k mbModel
  CC.Load model next → do
    case model of
      Card.FormInput (M.TextLike r) → do
        H.modify _{ formInputType = Just r.formInputType }
        H.query' CS.cpTextLike unit $ H.action $ TextLike.Load r
        pure next
      Card.FormInput (M.Labeled r) → do
        H.modify _{ formInputType = Just r.formInputType }
        H.query' CS.cpLabeled unit $ H.action $ Labeled.Load r
        pure next
      Card.FormInput M.Static → do
        H.modify _{ formInputType = Just FIT.Static }
        pure next
      _ →
        pure next
  CC.ReceiveInput input _ next → do
    case input of
      SetupTextLikeFormInput p → do
        H.modify _{ formInputType = Just p.formInputType }
        H.query' CS.cpTextLike unit $ H.action $ TextLike.Setup p
        pure next
      SetupLabeledFormInput p → do
        H.modify _{ formInputType = Just p.formInputType }
        H.query' CS.cpLabeled unit $ H.action $ Labeled.Setup p
        pure next
      CategoricalMetric metric → do
        H.modify _{ formInputType = Just FIT.Static }
        H.query' CS.cpMetric unit $ H.action $ Metric.SetMetric metric
        pure next
      _ →
        pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? _AutoSelect)
      $ H.query' CS.cpLabeled unit ∘ H.action ∘ Labeled.SetSelected
    pure next
  CC.ReceiveDimensions dims reply → do
    let
      widthPadding = 6
      intWidth = floor dims.width - widthPadding
      intHeight = floor dims.height
    H.modify (_ { dimensions = { width: intWidth, height: intHeight } })
    mbLod ← H.query' CS.cpMetric unit $ H.request Metric.GetLOD
    pure $ reply
      case mbLod of
        Nothing → if dims.width < 240.0 then Low else High
        Just lod → lod

evalComponent ∷ Query ~> DSL
evalComponent = case _ of
  RaiseUpdate next → do
    H.raise CC.modelUpdate
    pure next
