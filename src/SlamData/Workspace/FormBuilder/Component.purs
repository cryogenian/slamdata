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

module SlamData.Workspace.FormBuilder.Component
  ( Query(..)
  , Message(..)
  , ItemSlot(..)
  , formBuilderComponent
  , module SlamData.Workspace.FormBuilder.Component.State
  ) where

import SlamData.Prelude
import Data.Array as A
import Data.List as L
import Data.Unfoldable as U
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Render.CSS as RC
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.FormBuilder.Component.State (ItemId, State, addItem, emptyState, initialState, removeItem)
import SlamData.Workspace.FormBuilder.Item.Component as Item

-- | The query signature for the FormBuilder component
data Query a
  = GetItems (L.List Item.Model → a)
  | SetItems (L.List Item.Model) a
  | HandleItem ItemSlot Item.Message a
  | SetAccessType AT.AccessType a

data Message = ItemUpdated

newtype ItemSlot = ItemSlot ItemId
derive newtype instance eqItemSlot ∷ Eq ItemSlot
derive newtype instance ordItemSlot ∷ Ord ItemSlot

type DSL m = H.ParentDSL State Query Item.Query ItemSlot Message m
type HTML m = H.ParentHTML Query Item.Query ItemSlot m

formBuilderComponent ∷ ∀ m. H.Component HH.HTML Query Unit Message m
formBuilderComponent =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

eval ∷ ∀ m. Query ~> DSL m
eval = case _ of
  GetItems k → do
    { items } ← H.get
    -- the last item is always empty
    let properItems = L.init items # fromMaybe L.Nil
    k <<< L.catMaybes <$> for properItems \i →
      H.query (ItemSlot i) $ H.request Item.GetModel
  SetItems items next → do
    -- clear out the existing children
    accessType ← H.gets _.accessType
    H.put $ emptyState { accessType = accessType }
    let numExtra = if AT.isEditable accessType then 1 else 0
    U.replicateA (L.length items + numExtra) (H.modify addItem) ∷ DSL m (L.List Unit)

    let
      stepItem i m =
        H.query (ItemSlot i) (H.action (Item.SetModel m))
          $> i + 1

    -- step through `items` and initialize each child component
    void $ L.foldM stepItem 0 items
    pure next
  HandleItem slotId@(ItemSlot i) msg next → do
    case msg of
      Item.NameChanged "" → H.modify (removeItem i)
      _ → pure unit
    addItemIfNecessary slotId
    H.raise ItemUpdated
    pure next
  SetAccessType accessType next → do
    H.modify (_ { accessType = accessType })
    pure next

addItemIfNecessary ∷ ∀ m. ItemSlot → DSL m Unit
addItemIfNecessary (ItemSlot i) = do
  { nextId } ← H.get
  when (i == nextId - 1) $ H.modify addItem

render ∷ ∀ m. State → HTML m
render state = renderTable state.items
  where
    renderTable ∷ L.List ItemId → HTML m
    renderTable items =
      if L.length items > 0
        then
          HH.table
            [ HP.classes [ HH.ClassName "form-builder", RC.form ] ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ HH.text "Name" ]
                    , HH.th_ [ HH.text "Type" ]
                    , HH.th_ [ HH.text "Default value" ]
                    ]
                ]
            , HH.tbody_ $ A.fromFoldable (renderItem <$> items)
            ]
        else
          HH.text "No variables."


    renderItem ∷ ItemId → HTML m
    renderItem itemId =
      let slotId = ItemSlot itemId
      in HH.slot slotId Item.component unit (HE.input (HandleItem slotId))
