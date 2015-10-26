{-
Copyright 2015 SlamData, Inc.

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

module Optic.Index where

import Prelude
import Data.Array ((!!), updateAt)
import Data.Maybe (maybe, fromMaybe)

type TraversalP s a = forall f. (Applicative f) => (a -> f a) -> s -> f s

ix :: forall a. Int -> TraversalP (Array a) a
ix n a2fa as =
  (maybe
   (pure as)
   (\a -> (\a' -> fromMaybe as (updateAt n a' as)) <$> a2fa a)
   (as !! n))
