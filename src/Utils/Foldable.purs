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

module Utils.Foldable where

import SlamData.Prelude

import Data.List as L

enumeratedFor_
  ∷ ∀ a b f m ix
  . (Applicative m, Foldable f, Semiring ix)
  ⇒ f a
  → (ix × a → m b)
  → m Unit
enumeratedFor_ fl fn =
  snd
  $ foldr (\a (ix × action) →
            (ix + one)
            × ((fn $ ix × a) *> action))
    (zero × pure unit) fl

splitList
  ∷ ∀ a
  . Int
  → L.List a
  → L.List (L.List a)
splitList chunk inp =
  splitList' inp L.Nil
  where
  splitList' L.Nil out = out
  splitList' lst out =
    let
      taken = L.take chunk lst
      dropped = L.drop chunk lst
    in
      splitList' dropped $ L.Cons taken out

chunkedParTraverse
  ∷ ∀ f m t a b
  . (Parallel f m, Traversable t, Applicative m)
  ⇒ (a → m b)
  → t a
  → (t a → t (t a))
  → (t (t b) → t b)
  → m (t b)
chunkedParTraverse f ta split concat =
  map concat $  traverse (parTraverse f) $ split ta
