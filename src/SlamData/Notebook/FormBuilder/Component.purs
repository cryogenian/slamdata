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

module SlamData.Notebook.FormBuilder.Component
  ( Query(..)
  , ItemSlot()
  , StateP()
  , QueryP()
  , formBuilderComponent
  , module SlamData.Notebook.FormBuilder.Component.State
  ) where

import Prelude

import Control.Monad (when)

import Data.Functor
import Data.Functor.Coproduct
import Data.Generic as G
import Data.List as L
import Data.Maybe as M
import Data.NaturalTransformation
import Data.Traversable as Tr

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Notebook.FormBuilder.Component.State
import SlamData.Notebook.FormBuilder.Item.Component as Item

-- | The query signature for the FormBuilder component
data Query a
  = GetItems (L.List Item.Model -> a)
  | SetItems (L.List Item.Model) a

newtype ItemSlot = ItemSlot ItemId
derive instance genericItemSlot :: G.Generic ItemSlot
instance eqItemSlot :: Eq ItemSlot where eq = G.gEq
instance ordItemSlot :: Ord ItemSlot where compare = G.gCompare

type StateP g = InstalledState State Item.State Query Item.Query g ItemSlot
type QueryP = Coproduct Query (ChildF ItemSlot Item.Query)

type FormBuilderDSL g = ParentDSL State Item.State Query Item.Query g ItemSlot
type FormBuilderHTML g = ParentHTML Item.State Query Item.Query g ItemSlot

formBuilderComponent
  :: forall g
   . (Applicative g)
  => Component (StateP g) QueryP g
formBuilderComponent =
  parentComponent' render eval peek

eval
  :: forall g
   . (Applicative g)
  => Natural Query (FormBuilderDSL g)
eval (GetItems k) = do
  { items } <- get
  -- the last item is always empty
  let properItems = L.init items # M.fromMaybe L.Nil
  k <<< L.catMaybes <$> Tr.for properItems \i ->
    query (ItemSlot i) $ request Item.GetModel
eval (SetItems items next) = do
  -- clear out the existing children
  set emptyState
  L.replicateM (L.length items + 1) $
    modify addItem

  -- force a re-render in order for the child components to be update
  liftH $ liftH $ pure unit

  let
    stepItem i m =
      query (ItemSlot i) (action (Item.SetModel m))
        $> i + 1

  -- step through `items` and initialize each child component
  void $ L.foldM stepItem 0 items
  pure next

peek
  :: forall g a
   . ChildF ItemSlot Item.Query a
  -> FormBuilderDSL g Unit
peek (ChildF (ItemSlot i) q) =
  case q of
    Item.Update uq ->
      case uq of
        Item.UpdateName str _ -> do
          when (str == "") $
            modify $ removeItem i
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
      { nextId } <- get
      when (i == nextId - 1) $
        modify addItem


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
      H.table [ HP.class_ $ H.className "form-builder" ] $
        [ H.thead_
            [ H.tr_
                [ H.th_ [ H.text "Name" ]
                , H.th_ [ H.text "Type" ]
                , H.th_ [ H.text "Default" ]
                ]
            ]
        ] <> L.fromList (renderItem <$> items)

    renderItem
      :: ItemId
      -> FormBuilderHTML g
    renderItem itemId =
      H.slot (ItemSlot itemId) \_ ->
        { component: Item.itemComponent
        , initialState: Item.initialState
        }
