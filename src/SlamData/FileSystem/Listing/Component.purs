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

module SlamData.FileSystem.Listing.Component where

import SlamData.Prelude

import Data.Array (zipWith, range, length, cons, sortBy, filter, nub)
import Data.Lens ((.~), (%~), (<>~), lens, Lens')

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Monad (Slam)
import SlamData.FileSystem.Listing.Item (Item)
import SlamData.FileSystem.Listing.Item.Component as Item
import SlamData.Render.ClassName as CN

type State =
  { items ∷ Array Item
  , isSearching ∷ Boolean
  , isHidden ∷ Boolean
  }

initialState ∷ State
initialState =
  { items: [ ]
  , isSearching: false
  , isHidden: true
  }

_items ∷ Lens' State (Array Item)
_items = lens _.items _{items = _}

_isSearching ∷ Lens' State Boolean
_isSearching = lens _.isSearching _{isSearching = _}

_isHidden ∷ Lens' State Boolean
_isHidden = lens _.isHidden _{isHidden = _}

zipItems ∷ forall a. (Int → Item → a) → Array Item → Array a
zipItems f items = zipWith f (0 `range` length items) items

data Query a
  = Reset a
  | Add Item a
  | Adds (Array Item) a
  | SortBy (Item → Item → Ordering) a
  | SetIsSearching Boolean a
  | Filter (Item → Boolean) a
  | SetIsHidden Boolean a
  | Get (Array Item → a)
  | HandleItemAction ItemSlot Item.Message a
  | Remove Item a

data Message
  = Added (Array Item)
  | ItemMessage Item.Message

data ItemSlot = ItemSlot Int Item
derive instance eqItemSlot ∷ Eq ItemSlot
derive instance ordItemSlot ∷ Ord ItemSlot

type HTML = H.ParentHTML Query Item.Query ItemSlot Slam
type DSL = H.ParentDSL State Query Item.Query ItemSlot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → HTML
render state@{ items } =
  HH.div
    [ HP.classes [ CN.listGroup, CN.results ] ]
    $ zipItems (install state) items

eval ∷ Query ~> DSL
eval = case _ of
  Reset next →
    H.modify (_items .~ mempty) $> next
  Add item next → do
    H.modify (_items %~ nub <<< cons item)
    H.raise $ Added [ item ]
    pure next
  Adds items next → do
    H.modify (_items <>~ items)
    H.modify (_items %~ nub)
    H.raise $ Added items
    pure next
  SortBy sortFn next →
    H.modify (_items %~ sortBy sortFn) $> next
  SetIsSearching bool next →
    do
    H.modify (_isSearching .~ bool)
    items ← H.gets _.items
    for_ (zipItems ItemSlot items) \slot →
      H.query slot $ H.action $ Item.SetIsSearching bool
    pure next
  Filter filterFn next →
    H.modify (_items %~ filter filterFn) $> next
  SetIsHidden bool next →
    do
    H.modify (_isHidden .~ bool)
    items ← H.gets _.items
    for_ (zipItems ItemSlot items) \slot →
      H.query slot $ H.action $ Item.SetIsHidden bool
    pure next
  Get continue →
    continue <$> H.gets _.items
  HandleItemAction p msg next → do
    H.raise $ ItemMessage msg
    case msg of
      Item.Selected → do
        items ← H.gets _.items
        for_ (zipItems ItemSlot items) \slot →
          unless (slot == p) $ void $ H.query slot $ H.action Item.Deselect
      _ →
        pure unit
    pure next
  Remove item next → do
    H.modify (_items %~ filter (not ∘ eq item)) $> next

install ∷ State → Int → Item → HTML
install { isSearching, isHidden } ix item =
  let slotId = ItemSlot ix item
  in HH.slot
      slotId
      (Item.component
        { isSearching: isSearching
        , isHidden: isHidden
        , item: item
        })
      unit
      (HE.input (HandleItemAction slotId))
