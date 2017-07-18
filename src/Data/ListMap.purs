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

module Data.ListMap where

import SlamData.Prelude hiding (empty)

import Data.List as L
import Data.Unfoldable (class Unfoldable)

newtype ListMap k v = ListMap (L.List (k × v))

derive instance functorListMap ∷ Functor (ListMap k)

empty ∷ ∀ k v. ListMap k v
empty = ListMap L.Nil

lookup ∷ ∀ k v. (k → k → Boolean) → k → ListMap k v → Maybe v
lookup eq_ k (ListMap a) = map snd $ L.find (eq_ k ∘ fst) a

member ∷ ∀ k v. (k → k → Boolean) → k → ListMap k v → Boolean
member eq_ k lm = isJust $ lookup eq_ k lm

insert ∷ ∀ k v. (k → k → Boolean) → k → v → ListMap k v → ListMap k v
insert eq_ k v (ListMap lm) =
  ListMap $ L.Cons (k × v) $ L.filter (eq_ k ∘ fst) lm

fromFoldable ∷ ∀ f k v. Foldable f ⇒ f (k × v) → ListMap k v
fromFoldable = ListMap ∘ L.fromFoldable

toUnfoldable ∷ ∀ u k v. Unfoldable u ⇒ ListMap k v → u (k × v)
toUnfoldable (ListMap lm) = L.toUnfoldable lm

union ∷ ∀ k v. (k → k → Boolean) → ListMap k v → ListMap k v → ListMap k v
union eq_  (ListMap l) r =
  foldl foldFn r l
  where
  foldFn lm (k × v) =
    insert eq_ k v lm


type Module k v =
  { lookup ∷ k → ListMap k v → Maybe v
  , member ∷ k → ListMap k v → Boolean
  , insert ∷ k → v → ListMap k v → ListMap k v
  , empty ∷ ListMap k v
  , union ∷ ListMap k v → ListMap k v → ListMap k v
  }

openModule ∷ ∀ k v. (k → k → Boolean) → Module k v
openModule eq_ =
  { lookup: lookup eq_
  , member: member eq_
  , empty: empty
  , insert: insert eq_
  , union: union eq_
  }
