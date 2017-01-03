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

module Data.Map.Diff
  ( Diff(..)
  , diff
  , updated
  ) where

import SlamData.Prelude
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map

data Diff a
  = Removed a
  | Added a
  | Unchanged a
  | Changed a

diff
  ∷ ∀ k a
  . (Ord k, Eq a)
  ⇒ Map k a
  → Map k a
  → Map k (Diff a)
diff old new = remove (foldl go (old × Map.empty) (Map.toList new))
  where
    go (o × d) (k × v) = case Map.pop k o of
      Just (v' × o') | v ≡ v' →
        o' × Map.insert k (Unchanged v) d
      Just (v' × o') →
        o' × Map.insert k (Changed v) d
      Nothing →
        o × Map.insert k (Added v) d

    remove (o × d) =
      Map.union d (Removed <$> o)

updated
  ∷ ∀ k a
  . Ord k
  ⇒ Map k (Diff a)
  → List k
updated = List.mapMaybe go ∘ Map.toList
  where
    go (k × d) = case d of
      Removed _ → Just k
      Added _ → Just k
      Changed _ → Just k
      _ → Nothing
