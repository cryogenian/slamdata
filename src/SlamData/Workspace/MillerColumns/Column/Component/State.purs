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

module SlamData.Workspace.MillerColumns.Column.Component.State where

import SlamData.Prelude

import Data.List (List(..))

import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Column.Component.Query (Query)
import SlamData.Workspace.MillerColumns.Column.Component.Request (RequestId(..), LoadRequest)
import SlamData.Workspace.MillerColumns.Column.Component.ColumnWidth (ColumnWidth)

import Halogen.Component.Utils.Debounced (DebounceTrigger(..))

data ColumnState = Loading | Loaded

derive instance eqColumnState ∷ Eq ColumnState
derive instance ordColumnState ∷ Ord ColumnState

instance showColumnState ∷ Show ColumnState where
  show = case _ of
    Loading → "Loading"
    Loaded → "Loaded"

type State a i o =
  { items ∷ List a
  , state ∷ ColumnState
  , selected ∷ Maybe a
  , filterText ∷ String
  , nextOffset ∷ Maybe Int
  , lastLoadRequest ∷ Maybe LoadRequest
  , lastRequestId ∷ RequestId
  , filterTrigger ∷ DebounceTrigger (Query a i o) Slam
  , width ∷ ColumnWidth
  }

initialState ∷ ∀ a i o. ColumnWidth × Maybe a → State a i o
initialState (width × selected) =
  { items: Nil
  , state: Loading
  , selected
  , filterText: ""
  , nextOffset: Nothing
  , lastLoadRequest: Nothing
  , lastRequestId: RequestId 0
  , filterTrigger: DebounceTrigger (const (pure unit))
  , width
  }
