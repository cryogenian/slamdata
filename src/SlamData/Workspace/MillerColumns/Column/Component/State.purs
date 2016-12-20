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

module SlamData.Workspace.MillerColumns.Column.Component.State where

import SlamData.Prelude

import Data.List (List(..))

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Column.Options (LoadParams)
import SlamData.Workspace.MillerColumns.Column.Component.Query (Query, ItemQuery')

data ColumnState = Loading | Loaded

derive instance eqColumnState ∷ Eq ColumnState
derive instance ordColumnState ∷ Ord ColumnState

instance showColumnState ∷ Show ColumnState where
  show = case _ of
    Loading → "Loading"
    Loaded → "Loaded"

type State a i =
  { items ∷ List a
  , state ∷ ColumnState
  , selected ∷ Maybe a
  , filterText ∷ String
  , nextOffset ∷ Maybe Int
  , lastLoadParams ∷ Maybe (LoadParams i)
  , tick ∷ Int
  , filterTrigger ∷ Query a Unit → Slam Unit
  }

type State' a i s f =
  H.ParentState
    (State a i)
    s
    (Query a)
    (ItemQuery' a f)
    Slam
    i

initialState ∷ ∀ a i. State a i
initialState =
  { items: Nil
  , state: Loading
  , selected: Nothing
  , filterText: ""
  , nextOffset: Nothing
  , lastLoadParams: Nothing
  , tick: 0
  , filterTrigger: const (pure unit)
  }
