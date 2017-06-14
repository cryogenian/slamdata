module SlamData.Workspace.Card.Setups.Viz.Component
  ( component
  ) where

import SlamData.Prelude

import CSS as CSS

import Data.Lens ((^?), _Just)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Properties as HP
--import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as DS
import SlamData.Workspace.Card.Setups.Package.DSL as P
import SlamData.Workspace.Card.Setups.Package.Lenses as PL
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.Card.Setups.Viz.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Viz.Component.Query as Q
import SlamData.Workspace.Card.Setups.Viz.Component.State as ST
import SlamData.Workspace.Card.Setups.Viz.VizTypePicker as VT
import SlamData.Workspace.Card.CardType.VizType as VCT
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))


type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

package ∷ DS.Package
package = P.onPrism (M._BuildPie ∘ _Just) $ DS.interpret do
  category ←
    P.field PL._category PP._category
      >>= P.addSource _.category
      >>= P.addSource _.time
      >>= P.addSource _.date
      >>= P.addSource _.datetime

  value ←
    P.field PL._value PP._value
      >>= P.addSource _.value

  donut ←
    P.optional PL._donut PP._donut
      >>= P.addSource _.category
      >>= P.addSource _.time
      >>= P.isFilteredBy category
      >>= P.isActiveWhen category

  parallel ←
    P.optional PL._parallel PP._parallel
      >>= P.addSource _.category
      >>= P.addSource _.time
      >>= P.isFilteredBy category
      >>= P.isFilteredBy donut
      >>= P.isActiveWhen category

  pure unit


component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent CT.SetupViz $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div
  [ HCSS.style $ CSS.width (CSS.pct 100.0) *> CSS.height (CSS.pct 100.0) ]
  if state.vizTypePickerExpanded
    then [ picker ]
    else [ button, dims ]
  where
  button =
    HH.button
      [ HE.onClick $ HE.input_ $ right ∘ Q.ToggleVizPicker
      , HP.classes
          [ HH.ClassName "sd-viztype-button"
          ]
      ]
      [ HH.img
          [ HP.src $ VCT.darkIconSrc state.vizType ]
      , HH.p_ [ HH.text $ VCT.name state.vizType ]
      ]
  picker =
    HH.slot' CS.cpPicker unit VT.component unit
      $ HE.input \e → right ∘ Q.HandlePicker e
  dims =
    HH.slot' CS.cpDims unit (DM.component package) unit
      $ HE.input \e → right ∘ Q.HandleDims e

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    pure $ k $ M.SetupViz Nothing
  CC.Load m next →
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Axes) \axes → do
      H.modify _{ axes = Just axes }
      void $ H.query' CS.cpPicker unit $ H.action $ VT.UpdateAxes axes
    pure next
  CC.ReceiveDimensions _ reply →
    pure $ reply High

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.HandlePicker vt next → do
    case vt of
      VT.SetVizType v → do
        H.modify _{ vizType = v }
      _ → pure unit
    H.modify _{ vizTypePickerExpanded = false }
    H.raise CC.modelUpdate
    pure next
  Q.HandleDims dm next → do
    H.raise CC.modelUpdate
    pure next
  Q.ToggleVizPicker next → do
    H.modify _{ vizTypePickerExpanded = true }
    state ← H.get
    for_ state.axes \axes →
      void $ H.query' CS.cpPicker unit $ H.action $ VT.UpdateAxes axes
    pure next
