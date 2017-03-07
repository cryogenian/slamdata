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

module SlamData.Workspace.Card.StructureEditor.Component
  ( component
  , module SlamData.Workspace.Card.StructureEditor.Component.Query
  ) where

import SlamData.Prelude

import Data.List as L

import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.GlobalMenu.Bus as GMB
import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.StructureEditor.Column.Component as SECC
import SlamData.Workspace.Card.StructureEditor.Common (columnItemLabel, columnItemPath, columnPathIsLeaf, load, rootColumn)
import SlamData.Workspace.Card.StructureEditor.Component.ChildSlot as CS
import SlamData.Workspace.Card.StructureEditor.Component.Query (Query(..))
import SlamData.Workspace.Card.StructureEditor.Component.Query as Q
import SlamData.Workspace.Card.StructureEditor.Component.State as S
import SlamData.Workspace.Card.StructureEditor.Item.Component as SEIC
import SlamData.Workspace.Card.StructureEditor.Model (Model(..))
import SlamData.Workspace.LevelOfDetails as LOD
import SlamData.Workspace.MillerColumns.Component as MC

type DSL = CC.InnerCardParentDSL S.State Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Query CS.ChildQuery CS.ChildSlot

component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent CT.StructureEditor $ H.lifecycleParentComponent
    { render: render
    , eval: evalCard ⨁ evalStructureEditor
    , initialState: const S.initialState
    , receiver: const Nothing
    , initializer: Just $ right $ H.action Q.Init
    , finalizer: Nothing
    }

render ∷ S.State → HTML
render state =
  HH.div
    [ HP.class_ (HH.ClassName "sd-structure-editor-container") ]
    [ renderColumns ]

renderColumns ∷ HTML
renderColumns =
  HH.slot'
    CS.cpColumns
    unit
    (MC.component columnOptions)
    (rootColumn × L.Nil)
    (map right ∘ HE.input HandleColumnsMessage)

columnOptions ∷ SECC.ColumnOptions
columnOptions =
  MC.ColumnOptions
    { renderColumn: SECC.component
    , renderItem: SEIC.component
    , label: columnItemLabel
    , isLeaf: columnPathIsLeaf
    , id: columnItemPath
    }

evalStructureEditor ∷ Query ~> DSL
evalStructureEditor = case _ of
  Q.Init next → do
    { auth } ← Wiring.expose
    H.subscribe $ busEventSource (\msg → right (Q.HandleSignInMessage msg H.Listening)) auth.signIn
    pure next
  Q.HandleSignInMessage msg next → do
    case msg of
      GMB.SignInSuccess → void $ H.query' CS.cpColumns unit $ H.action $ MC.Reload
      _ → pure unit
    pure next
  Q.HandleColumnsMessage msg next → do
    case msg of
      Left (MC.LoadRequest (path × req)) → do
        res ← load path req =<< H.gets _.resource
        void $ H.query' CS.cpColumns unit $ H.action $ MC.FulfilLoadRequest (path × res)
      _ →
        pure unit
    pure next

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    mbRes ← H.get
    pure $ k $ Card.StructureEditor $ Model {}
  CC.Load (Card.StructureEditor model) next → do
    -- void $ H.query unit $ H.action $ MC.Populate $ unit × L.Nil
    pure next
  CC.Load _ next →
    pure next
  CC.ReceiveInput x y next → do
    H.modify \st → st { cycle = st.cycle + 1, resource = Port.extractFilePath y }
    _ ← H.query' CS.cpColumns unit $ H.action MC.Reload
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next → do
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 250.0 ∨ dims.height < 50.0
      then LOD.Low
      else LOD.High
