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

module SlamData.Prelude
  ( (∘), (⊕), (⋙), (⋘), (≡), (≠), (×), (≪), (≫), (=<<)
  , (∨), (∧), (⨁), (⊹)
  , flipCompose, applyRight, applyLeft
  , type (⨁), type (⊹), type (×)
  , module Prelude
  , module Control.Alt
  , module Control.Apply
  , module Control.Bind
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.MonadPlus
  , module Control.Plus
  , module Data.Bifoldable
  , module Data.Bifunctor
  , module Data.Bitraversable
  , module Data.Const
  , module Data.Either
  , module Data.Foldable
  , module Data.Functor
  , module Data.Functor.Coproduct
  , module Data.Generic
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Void
  , module Debug.Trace
  , module Partial.Unsafe
  )
  where

import Prelude hiding ((=<<))

import Control.Alt (class Alt, (<|>))
import Control.Apply ((*>), (<*))
import Control.Bind (join, (>=>), (<=<))
import Control.Bind as CB
import Control.Monad (when, unless)
import Control.Monad.Trans (class MonadTrans, lift)
import Control.MonadPlus (class MonadPlus, guard)
import Control.Plus (class Plus, empty)

import Data.Bifoldable (class Bifoldable, bitraverse_, bifor_)
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bitraverse, bisequence, bifor)
import Data.Const (Const(..), getConst)
import Data.Either (Either(..), either, isLeft, isRight, fromRight)
import Data.Foldable (class Foldable, traverse_, for_, foldMap, foldl, foldr, fold)
import Data.Functor (($>), (<$))
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', fromJust)
import Data.Monoid (class Monoid, mempty)
import Data.Traversable (class Traversable, traverse, sequence, for)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Void (Void, absurd)

import Debug.Trace (spy, trace, traceA, traceAny, traceAnyA, traceAnyM, traceShow, traceShowA, traceShowM)

import Partial.Unsafe (unsafePartial)

flipCompose ∷ ∀ a b c d. Semigroupoid a ⇒ a b c → a c d → a b d
flipCompose = flip compose

applyRight ∷ ∀ f a b. Apply f ⇒ f a → f b → f b
applyRight a b = const id <$> a <*> b

applyLeft ∷ ∀ f a b. Apply f ⇒ f a → f b → f a
applyLeft a b = const <$> a <*> b

-- TODO: `purescript-prelude` gets this wrong at the moment
infixr 1 CB.bindFlipped as =<<

infixr 9 compose as ∘
infixr 5 append as ⊕
infixr 9 flipCompose as ⋙
infixr 9 compose as ⋘
infixl 4 apply as ⊛
infix 4 eq as ≡
infix 4 notEq as ≠
infixr 1 Tuple as ×
infixl 4 applyRight as ≫
infixl 4 applyLeft as ≪
infixr 3 conj as ∧
infixr 2 disj as ∨

infixr 4 type Either as ⊹
infixr 4 type Coproduct as ⨁
infixr 5 either as ⊹
infixr 5 coproduct as ⨁

infixr 4 type Tuple as ×
