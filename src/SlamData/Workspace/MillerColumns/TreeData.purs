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

module SlamData.Workspace.MillerColumns.TreeData where

import SlamData.Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree as CF

import Data.Foldable (find)
import Data.List as L

import SlamData.Workspace.MillerColumns.Column.BasicFilter (mkFilter)
import SlamData.Workspace.MillerColumns.Component.State (ColumnsData)

type Tree a = CF.Cofree L.List a

-- | This is only suitable for use when every `a` in the tree is unique.
loadFromTree
  ∷ ∀ m a r
  . (Eq a, Applicative m)
  ⇒ (a → String)
  → Tree a
  → { path ∷ a, filter ∷ String | r }
  → m { items ∷ L.List a, nextOffset ∷ Maybe Int }
loadFromTree label tree { path, filter } =
  let
    labelFilter = mkFilter filter ∘ label
    items = L.filter labelFilter $ go $ pure tree
  in
    pure { items, nextOffset: Nothing }
  where
  go ∷ L.List (Tree a) → L.List a
  go branches =
    case find (\node → CF.head node == path) branches of
      Just subtree → extract <$> CF.tail subtree
      Nothing → CF.tail <$> branches >>= go

initialStateFromTree ∷ ∀ a. Tree a → ColumnsData a a
initialStateFromTree t = extract t × L.Nil
