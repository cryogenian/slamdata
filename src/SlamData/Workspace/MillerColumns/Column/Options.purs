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

module SlamData.Workspace.MillerColumns.Column.Options where

import SlamData.Prelude

import Data.List as L

import Halogen as H
import Halogen.HTML as HH

import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Column.Component.Query as Column
import SlamData.Workspace.MillerColumns.Column.Component.Item as Item

type LoadParams i = { path ∷ i, filter ∷ String, offset ∷ Maybe Int }

type ColumnComponent a i o =
  H.Component HH.HTML (Column.Query' a i o) (Maybe a) (Column.Message' a i o) Slam

type ItemComponent a o =
  H.Component HH.HTML (Item.Query a o) Item.State (Item.Message' a o) Slam

newtype ColumnOptions a i o =
  ColumnOptions
    { renderColumn ∷ ColumnOptions a i o → i → ColumnComponent a i o
    , renderItem ∷ i → a → ItemComponent a o
    , label ∷ a → String
    , load ∷ LoadParams i → Slam { items ∷ L.List a, nextOffset ∷ Maybe Int }
    , isLeaf ∷ i → Boolean
    , id ∷ a → i
    }
