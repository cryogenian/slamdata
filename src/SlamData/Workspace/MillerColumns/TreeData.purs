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
import Data.List ((:))
import Data.List as L

import SlamData.Workspace.MillerColumns.Component.State as S
import SlamData.Workspace.MillerColumns.Column.BasicFilter (mkFilter)

type Tree a = CF.Cofree L.List a

loadFromTree
  ∷ ∀ m a i r
  . (Eq i, Applicative m)
  ⇒ (a → i)
  → (a → String)
  → Tree a
  → { path ∷ L.List i, filter ∷ String | r }
  → m { items ∷ L.List a, nextOffset ∷ Maybe Int }
loadFromTree f label tree { path, filter } =
  pure $ go tree $ L.drop 1 $ L.reverse $ path
  where
  go ∷ Tree a → L.List i → { items ∷ L.List a, nextOffset ∷ Maybe Int }
  go subtree = case _ of
    x : xs →
      case find (\node → f (extract node) == x) (CF.tail subtree) of
        Nothing → { items: L.Nil, nextOffset: Nothing }
        Just subtree' → go subtree' xs
    _ →
      let
        labelFilter = mkFilter filter ∘ label
        items = L.filter labelFilter (extract <$> CF.tail subtree)
      in
        { items, nextOffset: Nothing }

initialStateFromTree ∷ ∀ a i. (a → i) → Tree a → S.State a i
initialStateFromTree f tree =
  S.initialState { path = L.singleton $ f $ extract tree }
