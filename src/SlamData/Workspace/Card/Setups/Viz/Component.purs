module SlamData.Workspace.Card.Setups.Viz.Component
  ( component
  ) where

import SlamData.Prelude

import CSS as CSS

import Data.Array as A
import Data.Lens ((^?))
import Data.ListMap as LM
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Properties as HP
import SlamData.Render.Icon as I
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.DimensionMap.Package as DP
import SlamData.Workspace.Card.Setups.Viz.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Viz.Component.Query as Q
import SlamData.Workspace.Card.Setups.Viz.Component.State as ST
import SlamData.Workspace.Card.Setups.VizPicker.Component as VT
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.CardType.VizType as VCT
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

lm ∷ ∀ a. LM.Module VCT.VizType a
lm = LM.openModule VCT.eq_

component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent CT.setupViz $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div
  [ HCSS.style $ CSS.width (CSS.pct 100.0) *> CSS.height (CSS.pct 100.0) ]
  $ ( if state.vizTypePickerExpanded
      then [ picker ]
      else [ button ] <> dims )
  ⊕ aux
  where
  icon ∷ Array HTML
  icon = foldMap (pure ∘ I.unIconHTML ∘ VCT.icon) state.vizType

  button =
    HH.button
      [ HE.onClick $ HE.input_ $ right ∘ Q.ToggleVizPicker
      , HP.classes
          [ HH.ClassName "sd-viztype-button"
          ]
      ]
      $ ( icon )
      ⊕ ( foldMap (pure ∘ HH.p_ ∘ pure ∘ HH.text ∘ VCT.name) state.vizType )

  picker =
    HH.slot' CS.cpPicker unit VT.component unit
      $ HE.input \e → right ∘ Q.HandlePicker e
  dims = fromMaybe [ ] do
    vt ← state.vizType
    package ← lm.lookup vt DP.packages
    pure $
      [ HH.slot' CS.cpDims unit DM.component package
        $ HE.input \e → right ∘ Q.HandleDims e
      ]
  aux = fromMaybe [ ] do
    vt ← state.vizType
    auxState ← lm.lookup vt state.auxes
    comp ← Aux.vizTypeAux vt
    pure
      $ A.singleton
      $ HH.div_
      $ A.singleton
      $ HH.slot' CS.cpAux unit comp auxState
      $ HE.input \e → right ∘ Q.HandleAux e


cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    pure $ k $ M.SetupViz
      { dimMaps: st.dimMaps
      , vizType: fromMaybe CT.pie st.vizType
      , auxes: st.auxes
      }
  CC.Load m next → do
    case m of
      M.SetupViz r → do
        H.modify _
          { dimMaps = r.dimMaps
          , vizType = Just r.vizType
          }
        for_ (lm.lookup r.vizType r.dimMaps) \dimMap → do
          void $ H.query' CS.cpDims unit $ H.action $ DQ.Load dimMap
          H.raise CC.modelUpdate
      _ → pure unit
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Axes) \axes → do
      H.modify _{ axes = Just axes }
      _ ← H.query' CS.cpPicker unit $ H.action $ VT.UpdateAxes axes
      _ ← H.query' CS.cpDims unit $ H.action $ DQ.SetAxes axes
      pure unit
    pure next
  CC.ReceiveDimensions _ reply →
    pure $ reply High

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.HandlePicker vt next → do
    H.modify _{ vizTypePickerExpanded = false }
    st ← H.get
    case vt of
      VT.SetVizType v → do
        for_ (lm.lookup v st.dimMaps) \dimMap →
          void $ H.query' CS.cpDims unit $ H.action $ DQ.Load dimMap

        H.modify _{ vizType = Just v }
        H.raise CC.modelUpdate
      _ → pure unit
    pure next
  Q.HandleDims msg next → do
    case msg of
      DQ.Update dimMap →
        H.modify \st → st{ dimMaps =
          maybe st.dimMaps (\vt → lm.insert vt dimMap st.dimMaps) st.vizType }
    H.raise CC.modelUpdate
    pure next
  Q.ToggleVizPicker next → do
    state ← H.get
    H.modify _{ vizTypePickerExpanded = true
              , vizType = Nothing
              }
    for_ state.axes \axes →
      void $ H.query' CS.cpPicker unit $ H.action $ VT.UpdateAxes axes
    pure next
  Q.HandleAux auxState next → do
    H.modify \st → st { auxes =
      maybe st.auxes (\vt → lm.insert vt auxState st.auxes) st.vizType }
    H.raise CC.modelUpdate
    pure next
