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
import Data.Lens ((.~), (%~), (<>~), lens, LensP)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.FileSystem.Listing.Item (Item)
import SlamData.FileSystem.Listing.Item.Component as Item
import SlamData.Render.CSS as Rc

type State =
  { items :: Array Item
  , isSearching :: Boolean
  , isHidden :: Boolean
  }

initialState :: State
initialState =
  { items: [ ]
  , isSearching: false
  , isHidden: true
  }

_items :: LensP State (Array Item)
_items = lens _.items _{items = _}

_isSearching :: LensP State Boolean
_isSearching = lens _.isSearching _{isSearching = _}

_isHidden :: LensP State Boolean
_isHidden = lens _.isHidden _{isHidden = _}

zipItems :: forall a. (Int -> Item -> a) -> Array Item -> Array a
zipItems f items = zipWith f (0 `range` length items) items

data Query a
  = Reset a
  | Add Item a
  | Adds (Array Item) a
  | SortBy (Item -> Item -> Ordering) a
  | SetIsSearching Boolean a
  | Filter (Item -> Boolean) a
  | SetIsHidden Boolean a
  | Get (Array Item -> a)

data ItemSlot = ItemSlot Int Item

instance eqItemSlot :: Eq ItemSlot where
  eq (ItemSlot ix it) (ItemSlot ix' it') =
    ix == ix' && it == it'

instance ordItemSlot :: Ord ItemSlot where
  compare (ItemSlot ix it) (ItemSlot ix' it') =
    compare ix ix' <> compare it it'

type StateP = H.ParentState State Item.State Query Item.Query Slam ItemSlot
type QueryP = Coproduct Query (H.ChildF ItemSlot Item.Query)

type HTML = H.ParentHTML Item.State Query Item.Query Slam ItemSlot
type DSL = H.ParentDSL State Item.State Query Item.Query Slam ItemSlot

comp :: H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just peek }

render :: State -> HTML
render state@{ items } =
  HH.div
    [ HP.classes [ B.listGroup, Rc.results ] ]
    $ zipItems (install state) items

eval :: Query ~> DSL
eval (Reset next) = H.modify (_items .~ mempty) $> next
eval (Add item next) = H.modify (_items %~ nub <<< cons item) $> next
eval (Adds items next) = do
  H.modify (_items <>~ items)
  H.modify (_items %~ nub)
  pure next
eval (SortBy sortFn next) = H.modify (_items %~ sortBy sortFn) $> next
eval (SetIsSearching bool next) = do
  H.modify (_isSearching .~ bool)
  items <- H.gets _.items
  for_ (zipItems ItemSlot items) \slot ->
    H.query slot $ H.action $ Item.SetIsSearching bool
  pure next
eval (Filter filterFn next) = H.modify (_items %~ filter filterFn) $> next
eval (SetIsHidden bool next) = do
  H.modify (_isHidden .~ bool)
  items <- H.gets _.items
  for_ (zipItems ItemSlot items) \slot ->
    H.query slot $ H.action $ Item.SetIsHidden bool
  pure next
eval (Get continue) = continue <$> H.gets _.items

peek :: forall x. H.ChildF ItemSlot Item.Query x -> DSL Unit
peek (H.ChildF p (Item.Toggle _)) = void do
  items <- H.gets _.items
  for_ (zipItems ItemSlot items) \slot ->
    if slot == p
    then pure $ pure unit
    else H.query slot $ H.action Item.Deselect
peek _ = pure unit

install :: State -> Int -> Item -> HTML
install { isSearching, isHidden } ix item =
  HH.slot (ItemSlot ix item) \_ ->
    { component: Item.comp
    , initialState:
        { isSearching: isSearching
        , isHidden: isHidden
        , item: item
        }
    }
