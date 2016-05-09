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
  , ItemSlot
  , StateP
  , QueryP
  , formBuilderComponent
  , module SlamData.Workspace.FormBuilder.Component.State
  ) where

import SlamData.Prelude

import Data.List as L

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Render.CSS as RC
import SlamData.Workspace.FormBuilder.Component.State (ItemId, State, addItem, emptyState, initialState, removeItem)
import SlamData.Workspace.FormBuilder.Item.Component as Item

-- | The query signature for the FormBuilder component
data Query a
  = GetItems (L.List Item.Model -> a)
  | SetItems (L.List Item.Model) a

newtype ItemSlot = ItemSlot ItemId
derive instance genericItemSlot :: Generic ItemSlot
instance eqItemSlot :: Eq ItemSlot where eq = gEq
instance ordItemSlot :: Ord ItemSlot where compare = gCompare

type StateP g = H.ParentState State Item.State Query Item.Query g ItemSlot
type QueryP = Coproduct Query (H.ChildF ItemSlot Item.Query)

type FormBuilderDSL g = H.ParentDSL State Item.State Query Item.Query g ItemSlot
type FormBuilderHTML g = H.ParentHTML Item.State Query Item.Query g ItemSlot

formBuilderComponent
  :: forall g
   . (Applicative g)
  => H.Component (StateP g) QueryP g
formBuilderComponent =
  H.parentComponent { render, eval, peek: Just peek }

eval
  :: forall g
   . (Applicative g)
  => Natural Query (FormBuilderDSL g)
eval (GetItems k) = do
  { items } <- H.get
  -- the last item is always empty
  let properItems = L.init items # fromMaybe L.Nil
  k <<< L.catMaybes <$> for properItems \i ->
    H.query (ItemSlot i) $ H.request Item.GetModel
eval (SetItems items next) = do
  -- clear out the existing children
  H.set emptyState
  L.replicateM (L.length items + 1) $
    H.modify addItem

  let
    stepItem i m =
      H.query (ItemSlot i) (H.action (Item.SetModel m))
        $> i + 1

  -- step through `items` and initialize each child component
  void $ L.foldM stepItem 0 items
  pure next

peek
  :: forall g a
   . H.ChildF ItemSlot Item.Query a
  -> FormBuilderDSL g Unit
peek (H.ChildF (ItemSlot i) q) =
  case q of
    Item.Update uq ->
      case uq of
        Item.UpdateName str _ -> do
          when (str == "") $
            H.modify $ removeItem i
          addItemIfNecessary
        Item.UpdateFieldType _ _ ->
          addItemIfNecessary
        Item.UpdateDefaultValue _ _ ->
          addItemIfNecessary
    Item.SetModel _ _ ->
      addItemIfNecessary
    _ ->
      pure unit

  where
    addItemIfNecessary = do
      { nextId } <- H.get
      when (i == nextId - 1) $
        H.modify addItem


render
  :: forall g
   . (Functor g)
  => State
  -> FormBuilderHTML g
render { items } =
  renderTable items

  where
    renderTable
      :: L.List ItemId
      -> FormBuilderHTML g
    renderTable items =
      HH.table
        [ HP.classes [ HH.className "form-builder", RC.form ] ]
        [ HH.thead_
            [ HH.tr_
                [ HH.th_ [ HH.text "Name" ]
                , HH.th_ [ HH.text "Type" ]
                , HH.th_ [ HH.text "Default value" ]
                ]
            ]
        , HH.tbody_ $ L.fromList (renderItem <$> items)
        ]

    renderItem
      :: ItemId
      -> FormBuilderHTML g
    renderItem itemId =
      HH.slot (ItemSlot itemId) \_ ->
        { component: Item.itemComponent
        , initialState: Item.initialState
        }
