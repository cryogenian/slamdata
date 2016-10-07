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
import Control.Comonad.Cofree ((:<))
import Control.Comonad.Cofree as CF

import Data.Foldable (find)

import Data.List ((:))
import Data.List as L

import SlamData.Workspace.MillerColumns.Component.State (State)

type Tree a = CF.Cofree L.List a

loadFromTree
  ∷ ∀ m a i
  . (Eq i, Applicative m)
  ⇒ (a → i)
  → Tree a
  → L.List i
  → m (Maybe (L.List a))
loadFromTree f tree = pure ∘ go tree ∘ L.drop 1 ∘ L.reverse
  where
  go ∷ Tree a → L.List i → Maybe (L.List a)
  go subtree = case _ of
    x : xs →
      case find (\node → f (extract node) == x) (CF.tail subtree) of
        Nothing → Nothing
        Just subtree' → go subtree' xs
    _ →
      let items = extract <$> CF.tail subtree
      in if L.null items then Nothing else Just items

initialStateFromTree ∷ ∀ a i. (a → i) → Tree a → State a i
initialStateFromTree f tree =
  { element: Nothing
  , columns: [Tuple (f (extract tree)) (extract <$> CF.tail tree)]
  , cycle: 0
  , selected: L.Nil
  }
