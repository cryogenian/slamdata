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

module SlamData.Monad.ForkF where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error)

import Unsafe.Coerce (unsafeCoerce)

data ForkF eff f x a = ForkF (f x) ((Error → Aff eff Boolean) → a)

fork ∷ ∀ eff f x. f x → Fork f (Error → Aff eff Boolean)
fork fx = mkFork $ ForkF fx id

data Fork (f ∷ Type → Type) a

mkFork ∷ ∀ eff f x a. ForkF eff f x a → Fork f a
mkFork = unsafeCoerce

unFork ∷ ∀ f a r. (∀ eff x. ForkF eff f x a → r) → Fork f a → r
unFork = unsafeCoerce

instance functorFork ∷ Functor (Fork f) where
  map f = unFork \(ForkF fx c) → mkFork $ ForkF fx (map f c)

hoistFork ∷ ∀ f g a. (f ~> g) → Fork f a → Fork g a
hoistFork nat = unFork \(ForkF fx c) → mkFork $ ForkF (nat fx) c
