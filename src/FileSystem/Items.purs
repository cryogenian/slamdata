{-
Copyright 2015 SlamData, Inc.

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

-- | This module probably should be very generic and
-- | placed in separate project like `purescript-halogen-utils` or something
-- | and should be parameterized by child input type.
module FileSystem.Items where

import Prelude

import Data.Array (zipWith, range, length, cons, sortBy, filter, nub)
import Data.Foldable (for_)
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic)
import Data.Lens ((.~), (%~), (^.), (<>~), lens, LensP())
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)

import Halogen
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B

import FileSystem.Common (Slam())
import FileSystem.Item as Item
import Model.Item as Item
import Render.CssClasses as Rc

type StateRec =
  { items :: Array Item.Item
  , isSearching :: Boolean
  , isHidden :: Boolean
  }
newtype State = State StateRec

initialState :: State
initialState =
  State { items: [ ]
        , isSearching: false
        , isHidden: true
        }

_State :: LensP State StateRec
_State = lens (\(State r) -> r) (const State)

_items :: LensP State (Array Item.Item)
_items = _State <<< lens _.items _{items = _}

_isSearching :: LensP State Boolean
_isSearching = _State <<< lens _.isSearching _{isSearching = _}

_isHidden :: LensP State Boolean
_isHidden = _State <<< lens _.isHidden _{isHidden = _}

data Query a
  = Reset a
  | Add Item.Item a
  | Adds (Array Item.Item) a
  | SortBy (Item.Item -> Item.Item -> Ordering) a
  | SetIsSearching Boolean a
  | Filter (Item.Item -> Boolean) a
  | SetIsHidden Boolean a

data ItemSlot = ItemSlot Int Item.Item

instance eqItemSlot :: Eq ItemSlot where
  eq (ItemSlot ix it) (ItemSlot ix' it') =
    ix == ix' && it == it'

instance ordItemSlot :: Ord ItemSlot where
  compare (ItemSlot ix it) (ItemSlot ix' it') =
    case compare ix ix' of
      LT -> LT
      GT -> GT
      EQ -> compare it it'

type StateP =
  InstalledState State Item.State Query Item.Query Slam ItemSlot
type QueryP = Coproduct Query (ChildF ItemSlot Item.Query)


comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: RenderParent State Item.State Query Item.Query Slam ItemSlot
render state@(State{items = items}) =
  H.div [ P.classes [ B.listGroup, Rc.results ] ]
  $ zipWith (install state) (0 `range` length items) items

eval :: EvalParent Query State Item.State Query Item.Query Slam ItemSlot
eval (Reset next) = do
  modify (_items .~ mempty)
  pure next
eval (Add item next) = do
  modify (_items %~ nub <<< cons item)
  pure next
eval (Adds items next) = do
  modify (_items <>~ items)
  modify (_items %~ nub)
  pure next
eval (SortBy sortFn next) = do
  modify (_items %~ sortBy sortFn)
  pure next
eval (SetIsSearching bool next) = do
  modify (_isSearching .~ bool)
  items <- gets (^. _items)
  for_ (zipWith ItemSlot (0 `range` length items) items) \slot ->
    query slot $ action $ Item.SetIsSearching bool
  pure next
eval (Filter filterFn next) = do
  modify (_items %~ filter filterFn)
  pure next
eval (SetIsHidden bool next) = do
  modify (_isHidden .~ bool)
  items <- gets (^. _items)
  for_ (zipWith ItemSlot (0 `range` length items) items) \slot ->
    query slot $ action $ Item.SetIsHidden bool
  liftH $ pure unit
  pure next

peek :: Peek (ChildF ItemSlot Item.Query)
        State Item.State Query Item.Query Slam ItemSlot
peek (ChildF p (Item.Toggle _)) = void do
  items <- gets (^. _items)
  for_ (zipWith ItemSlot (0 `range` length items) items) \slot ->
    if slot == p
    then pure $ pure unit
    else query slot $ action $ Item.Deselect
peek _ = pure unit


install ::
  State -> Int -> Item.Item -> ParentHTML Item.State Query Item.Query Slam ItemSlot
install (State s) ix item =
  H.slot (ItemSlot ix item)
  \_ -> { component: Item.comp
        , initialState: Item.State { isSearching: s.isSearching
                                   , isHidden: s.isHidden
                                   , item: item
                                   , mbURI: Nothing
                                   }
        }
