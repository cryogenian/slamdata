{-
Copyright 2017 SlamData, Inc.

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

module SlamData.AdminUI.Group where

import SlamData.Prelude

import Data.Array as Array
import Data.List as L
import Data.Path.Pathy as Pathy
import Halogen as H
import Halogen.Component.Proxy (proxyQL)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ
import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Types as AT
import SlamData.AdminUI.Group.Item as GI
import SlamData.Monad (Slam)
import SlamData.Quasar.Security (groupInfo)
import SlamData.Workspace.MillerColumns.Column.Component as MCC
import SlamData.Workspace.MillerColumns.Column.Component.Request as MCREQ
import SlamData.Workspace.MillerColumns.Component as Miller
import SlamData.Workspace.MillerColumns.Component.State (ColumnsData)
import Unsafe.Coerce (unsafeCoerce)
import Utils.Path (rootFile)

type ColumnOptions = MCC.ColumnOptions AT.GroupItem AT.GroupIndex AT.GroupMessage
type ColumnQuery = MCC.Query AT.GroupItem AT.GroupIndex AT.GroupMessage

type Message' = MCC.Message' AT.GroupItem AT.GroupIndex AT.GroupMessage

component
  ∷ ColumnOptions
  → AT.GroupIndex
  → H.Component HH.HTML (MCC.Query' AT.GroupItem AT.GroupIndex AT.GroupMessage) (Maybe AT.GroupItem) Message' Slam
component opts = proxyQL ∘ component' opts

data Query a = Raise Message' a

type Query' = Coproduct ColumnQuery Query

type State = Maybe AT.GroupItem

type DSL = H.ParentDSL State Query' ColumnQuery Unit Message' Slam
type HTML = H.ParentHTML Query' ColumnQuery Unit Slam

component'
  ∷ ColumnOptions
  → AT.GroupIndex
  → H.Component HH.HTML Query' (Maybe AT.GroupItem) Message' Slam
component' opts path =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: Just ∘ left ∘ H.action ∘ MCC.SetSelection
    }
  where

  column ∷ H.Component HH.HTML ColumnQuery (Maybe AT.GroupItem) Message' Slam
  column = MCC.component' opts path

  render ∷ State → HTML
  render st =
    HH.div_
      [ HH.div
          [ HP.class_ (HH.ClassName "sd-structure-editor-column") ]
          [ HH.slot unit column st (Just ∘ right ∘ H.action ∘ Raise)
          , HH.div
              [ HP.class_ (HH.ClassName "sd-admin-ui-group-item") ]
              [ HH.text "Hello" ]
          ]
      ]

  eval ∷ Query' ~> DSL
  eval = coproduct evalInner evalOuter

  evalInner ∷ ColumnQuery ~> DSL
  evalInner =
    maybe (HQ.halt "Inner component query failed") pure <=< H.query unit

  evalOuter ∷ Query ~> DSL
  evalOuter = case _ of
    Raise msg next → do
      H.raise msg
      pure next

renderGroupsForm ∷ AT.GroupsState → Array AT.HTML
renderGroupsForm (AT.GroupsState _) =
  [ HH.slot' AT.cpGroups unit (Miller.component columnOptions) columnState (HE.input (either AT.HandleColumns AT.HandleColumnOrItem)) ]
  where
    columnState ∷ ColumnsData AT.GroupItem AT.GroupIndex
    columnState = Right rootFile × L.Nil

    columnOptions ∷ Miller.ColumnOptions AT.GroupItem AT.GroupIndex AT.GroupMessage
    columnOptions =
      Miller.ColumnOptions
        { renderColumn: component
        , renderItem: GI.component
        , label: AT.groupItemName
        , isLeaf: isLeft
        , id: case _ of
            AT.Group { path, isLeaf } → if isLeaf then Left (Left path) else Right path
            AT.User { path, id } → Left (Right (id × path))
        }

load
  ∷ Tuple AT.GroupIndex { requestId ∷ MCREQ.RequestId, filter ∷ String, offset ∷ Maybe Int }
  → AT.DSL (MCREQ.LoadResponse AT.GroupItem)
load (Left _ × { requestId }) = pure (noResult requestId)
load (Right path × { requestId }) =
  groupInfo path >>= case _ of
    Right { subGroups, members } → do
      items ← traverse groupFromPath subGroups
      let ms = map mkUserItem members
      pure { requestId
           , items: L.fromFoldable (ms <> items)
           , nextOffset: Nothing
           }
    Left e → pure (noResult requestId)
  where
    mkUserItem ∷ QA.UserId → AT.GroupItem
    mkUserItem id = AT.User { path, id, name: QA.runUserId id }

    groupFromPath ∷ Pathy.AbsFile Pathy.Sandboxed → AT.DSL AT.GroupItem
    groupFromPath p = do
      groupInfo p >>= case _ of
        Right { subGroups, members } →
          pure (AT.Group
            { path: p
            , name: Pathy.runFileName (Pathy.fileName p)
            , isLeaf: Array.null subGroups && Array.null members
            })
        Left e →
          -- TODO(Christoph): Ahem
          unsafeCoerce unit

noResult ∷ MCREQ.RequestId → MCREQ.LoadResponse AT.GroupItem
noResult requestId = { requestId, items: L.Nil, nextOffset: Nothing }
