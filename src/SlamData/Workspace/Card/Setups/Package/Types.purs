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

module SlamData.Workspace.Card.Setups.Package.Types
  ( Package
  , DimensionMap
  , emptyDimMap
  , getProjection
  , hasProjection
  , AxesProjection
  , packAxesProjection
  , unpackAxesProjection
  , Field
  , mandatoryField
  , optionalField
  , _guards
  , _filters
  , _projection
  , _axesPrjs
  , _lens
  , PackageFF(..)
  , PackageF
  , PackageMF
  , PackageM
  , AxesComposer
  , Reflection
  , idReflection
  , Projection
  , packProjection
  , unpackProjection
  , module Ax
  , module D
  )where

import SlamData.Prelude

import Control.Monad.Free (Free)

import Data.Lens (ALens', Lens', wander, lens, (^.))
import Data.Lens.At (class At)
import Data.Lens.Index (class Index)
import Data.List as L
import Data.StrMap as SM

import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Dimension (LabeledJCursor) as D

import Unsafe.Coerce (unsafeCoerce)

type Package m s =
  { allFields ∷ DimensionMap → Ax.AxisTypeAnnotated s () → L.List Projection
  , cursorMap ∷ DimensionMap → Ax.AxisTypeAnnotated s () → SM.StrMap s
  , save ∷ DimensionMap → m → Maybe m
  , load ∷ Maybe m → DimensionMap → DimensionMap
  }

type DimensionMap = SM.StrMap D.LabeledJCursor
emptyDimMap ∷ DimensionMap
emptyDimMap = SM.empty

getProjection ∷ DimensionMap → Projection → Maybe D.LabeledJCursor
getProjection dimMap prj = dimMap ^. unpackProjection prj

hasProjection ∷ DimensionMap → Projection → Boolean
hasProjection dimMap prj = isJust $ getProjection dimMap prj


data AxesProjection

packAxesProjection ∷ ∀ s r. (Ax.AxisTypeAnnotated s r → s) → AxesProjection
packAxesProjection = unsafeCoerce

unpackAxesProjection ∷ ∀ s r. AxesProjection → Ax.AxisTypeAnnotated s r → s
unpackAxesProjection = unsafeCoerce

type ProjectionU m a b = At m a b ⇒ Lens' m (Maybe b)
data Projection

packProjection ∷ ∀ m a b. ProjectionU m a b → Projection
packProjection = unsafeCoerce

unpackProjection ∷ ∀ m a b. Projection → ProjectionU m a b
unpackProjection = unsafeCoerce

type Field m =
  { projection ∷ Projection
  , lens ∷ (ALens' m D.LabeledJCursor) ⊹ (ALens' m (Maybe D.LabeledJCursor))
  , axesPrjs ∷ L.List AxesProjection
  , guards ∷ L.List Projection
  , filters ∷ L.List Projection
  }

mandatoryField ∷ ∀ m. ALens' m D.LabeledJCursor → Projection → Field m
mandatoryField lens projection =
  { lens: Left lens
  , projection
  , axesPrjs: L.Nil
  , guards: L.Nil
  , filters: L.Nil
  }

optionalField ∷ ∀ m. ALens' m (Maybe D.LabeledJCursor) → Projection → Field m
optionalField lens projection =
  { lens: Right lens
  , projection
  , axesPrjs: L.Nil
  , guards: L.Nil
  , filters: L.Nil
  }

_projection ∷ ∀ m. Lens' (Field m) Projection
_projection = lens _.projection _{ projection = _ }

_lens ∷ ∀ m. Lens' (Field m) ((ALens' m D.LabeledJCursor) ⊹ (ALens' m (Maybe D.LabeledJCursor)))
_lens = lens _.lens _{ lens = _ }

_axesPrjs ∷ ∀ m. Lens' (Field m) (L.List AxesProjection)
_axesPrjs = lens _.axesPrjs _{ axesPrjs = _ }

_guards ∷ ∀ m. Lens' (Field m) (L.List Projection)
_guards = lens _.guards _{ guards = _ }

_filters ∷ ∀ m. Lens' (Field m) (L.List Projection)
_filters = lens _.filters _{ filters = _ }

data PackageFF f m next
  = MandatoryField (ALens' m D.LabeledJCursor) Projection (f → next)
  | OptionalField (ALens' m (Maybe D.LabeledJCursor)) Projection (f → next)
  | Source f AxesProjection (f → next)
  | Depends { filter ∷ f, source ∷ f } (f → next)
  | ActiveGuard { guard ∷ f, source ∷ f } (f → next)

type PackageF m = PackageFF (Field m) m

type PackageMF f m = Free (PackageFF f m)

type PackageM m = Free (PackageF m)

type AxesComposer s =
  { guard ∷ Projection → DimensionMap → s → s
  , filter ∷ Projection → DimensionMap → s → s
  }

-- | Datatype used with `at` and `ix` and returns `Just index`
-- | e.g. `idReflection ^? ix 2 ≡ Just 2`
-- | `idReflection ^. at foo ≡ Just foo`
newtype Reflection a = Reflection (a → a)
derive instance newtypeReflection ∷ Newtype (Reflection a) _

instance reflectionIndex ∷ Index (Reflection a) a a where
  ix a = wander \f (Reflection fn) →
    map (Reflection ∘ const) $ f $ fn a

instance reflectionAt ∷ At (Reflection a) a a where
  at a = lens get set
    where
    get (Reflection f) = Just $ f a
    set st _ = st

idReflection ∷ ∀ a. Reflection a
idReflection = Reflection id
