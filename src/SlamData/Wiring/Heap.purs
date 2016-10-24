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

module SlamData.Wiring.Heap
  ( Cell
  , Heap
  , inc
  , dec
  , module SlamData.Wiring.Cache
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVAR, takeVar, putVar)
import Control.Monad.Aff.Free (class Affable, fromAff)

import Data.Map as Map

import SlamData.Wiring.Cache (Cache)
import SlamData.Wiring.Cache as Cache

type Cell a = Tuple Int a

type Heap k v = Cache k (Cell v)

inc
  ∷ ∀ eff m k v
  . (Affable (avar ∷ AVAR | eff) m, Ord k)
  ⇒ k
  → Heap k v
  → m (Maybe Int)
inc = modifyRefCount (_ + 1)

dec
  ∷ ∀ eff m k v
  . (Affable (avar ∷ AVAR | eff) m, Ord k)
  ⇒ k
  → Heap k v
  → m (Maybe Int)
dec = modifyRefCount (_ - 1)

modifyRefCount
  ∷ ∀ eff m k v
  . (Affable (avar ∷ AVAR | eff) m, Ord k)
  ⇒ (Int → Int)
  → k
  → Heap k v
  → m (Maybe Int)
modifyRefCount f key cache = fromAff do
  let avar = Cache.unCache cache
  vals ← takeVar avar
  case Map.lookup key vals of
    Just (Tuple n val) → do
      let n' = f n
      putVar avar (Map.insert key (Tuple n' val) vals)
        $> Just n'
    Nothing →
      putVar avar vals
        $> Nothing
