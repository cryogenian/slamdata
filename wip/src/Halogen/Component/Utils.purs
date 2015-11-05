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

module Halogen.Component.Utils where

import Prelude

import Control.Monad.Free (Free())
import Halogen
import Halogen.Component (ChildF(..))

applyCF :: forall a b c i. (a -> b i -> c) -> ChildF a b i -> c
applyCF fn (ChildF a b) = fn a b

forceRerender :: forall s f g. (Applicative g) => Free (HalogenF s f g) Unit
forceRerender = liftH $ pure unit

forceRerender' ::
  forall s s' f f' g p. (Applicative g) =>
  Free (HalogenF s f (QueryF s s' f f' g p)) Unit
forceRerender' = liftH $ liftH $ pure unit
