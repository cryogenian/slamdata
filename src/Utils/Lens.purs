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

module Utils.Lens where

import Prelude
import Data.Lens (Traversal', wander)
import Data.Traversable (traverse, class Traversable)
import Data.Tuple (Tuple(..))

lookup ∷ ∀ f a b. (Traversable f, Eq a) ⇒ a → Traversal' (f (Tuple a b)) b
lookup key = wander \f → traverse case _ of
  Tuple a b | a == key → Tuple a <$> f b
  pair → pure pair
