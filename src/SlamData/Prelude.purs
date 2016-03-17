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
  ( module Prelude
  , module Control.Alt
  , module Control.Apply
  , module Control.Bind
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.MonadPlus
  , module Control.Plus
  , module Data.Bifunctor
  , module Data.Const
  , module Data.Either
  , module Data.Foldable
  , module Data.Functor
  , module Data.Functor.Coproduct
  , module Data.Generic
  , module Data.Maybe
  , module Data.Monoid
  , module Data.NaturalTransformation
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Void
  )
  where

import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class BoundedOrd, class Category, class DivisionRing, class Eq, class Functor, class ModuloSemiring, class Monad, class Num, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, Unit, Ordering(..), add, ap, append, apply, asTypeOf, bind, bottom, compare, compose, conj, const, disj, div, eq, flip, id, liftA1, liftM1, map, mod, mul, negate, not, one, otherwise, pure, return, show, sub, top, unit, void, zero, (#), ($), (&&), (*), (+), (++), (-), (/), (/=), (<), (<#>), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>=), (>>=), (>>>), (||))

import Control.Alt (class Alt, (<|>))
import Control.Apply ((*>), (<*))
import Control.Bind (join, (=<<), (>=>), (<=<))
import Control.Monad (when, unless)
import Control.Monad.Trans (class MonadTrans, lift)
import Control.MonadPlus (class MonadPlus, guard)
import Control.Plus (class Plus, empty)

import Data.Bifunctor (bimap, lmap, rmap)
import Data.Const (Const(..), getConst)
import Data.Either (Either(..), either, isLeft, isRight)
import Data.Foldable (class Foldable, traverse_, for_, foldMap, foldl, foldr, fold)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Generic (class Generic, gEq, gCompare)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe')
import Data.Monoid (class Monoid, mempty)
import Data.NaturalTransformation (Natural)
import Data.Traversable (class Traversable, traverse, sequence, for)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Void (Void, absurd)
