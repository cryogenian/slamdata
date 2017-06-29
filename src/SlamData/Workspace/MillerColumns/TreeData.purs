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

module SlamData.Workspace.MillerColumns.TreeData where

import SlamData.Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree as CF

import Data.Foldable (find)
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Unfoldable (unfoldr)

import SlamData.Workspace.MillerColumns.Column.BasicFilter (mkFilter)
import SlamData.Workspace.MillerColumns.Column.Component.Request (LoadRequest, LoadResponse)
import SlamData.Workspace.MillerColumns.Component.State (ColumnsData)

type Tree a = CF.Cofree L.List a

-- | Construct a tree from an unfold function, root value, and list of values
-- | to unfold.
constructTree ∷ ∀ a b. Ord b ⇒ (a → Maybe (Tuple b a)) → b → L.List a → Tree b
constructTree unfold root as =
  let
    edges = (\bs → L.zip (L.drop 1 bs `L.snoc` root) bs) ∘ unfoldr unfold =<< as
    graph = foldr (\(Tuple k v) → M.alter (Just ∘ maybe (S.singleton v) (S.insert v)) k) M.empty edges
  in
    CF.unfoldCofree id (maybe L.Nil L.fromFoldable ∘ flip M.lookup graph) root

-- | This is only suitable for use when every `a` in the tree is unique.
loadFromTree
  ∷ ∀ a
  . Eq a
  ⇒ (a → String)
  → Tree a
  → a × LoadRequest
  → a × LoadResponse a
loadFromTree label tree (path × { requestId, filter }) =
  let
    labelFilter = mkFilter filter ∘ label
    items = L.filter labelFilter $ go $ pure tree
  in
    path × { requestId, items, nextOffset: Nothing }
  where
  go ∷ L.List (Tree a) → L.List a
  go branches =
    case find (\node → CF.head node == path) branches of
      Just subtree → extract <$> CF.tail subtree
      Nothing → CF.tail <$> branches >>= go

initialStateFromTree ∷ ∀ a. Tree a → ColumnsData a a
initialStateFromTree t = extract t × L.Nil
