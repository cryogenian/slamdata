module Data.ListMap where

import SlamData.Prelude hiding (empty)

import Data.Array as A
import Data.Unfoldable (class Unfoldable)

newtype ListMap k v = ListMap (Array (k × v))

derive instance functorListMap ∷ Functor (ListMap k)

empty ∷ ∀ k v. ListMap k v
empty = ListMap [ ]

lookup ∷ ∀ k v. (k → k → Boolean) → k → ListMap k v → Maybe v
lookup eq_ k (ListMap a) = map snd $ A.find (eq_ k ∘ fst) a

member ∷ ∀ k v. (k → k → Boolean) → k → ListMap k v → Boolean
member eq_ k lm = isJust $ lookup eq_ k lm

insert ∷ ∀ k v. (k → k → Boolean) → k → v → ListMap k v → ListMap k v
insert eq_ k v (ListMap lm) =
  ListMap $ A.cons (k × v) $ A.filter (eq_ k ∘ fst) lm

fromFoldable ∷ ∀ f k v. Foldable f ⇒ f (k × v) → ListMap k v
fromFoldable = ListMap ∘ A.fromFoldable

toUnfoldable ∷ ∀ u k v. Unfoldable u ⇒ ListMap k v → u (k × v)
toUnfoldable (ListMap lm) = A.toUnfoldable lm

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
