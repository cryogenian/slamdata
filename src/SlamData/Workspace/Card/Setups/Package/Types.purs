module SlamData.Workspace.Card.Setups.Package.Types
  ( Package
  , DimensionMap
  , emptyDimMap
  , AnyLens
  , packAnyLens
  , unpackAnyLens
  , AxesProjection
  , packAxesProjection
  , unpackAxesProjection
  , Field
  , newField
  , _mandatory
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

import Data.Lens (Lens, Lens', wander, lens)
import Data.Lens.At (class At)
import Data.Lens.Index (class Index)
import Data.List as L
import Data.StrMap as SM
import Data.Lens.Iso.Newtype (_Newtype)

import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Dimension (LabeledJCursor) as D

import Unsafe.Coerce (unsafeCoerce)

type Package m s =
  { allFields ∷ DimensionMap → Ax.AxisTypeAnnotated s → L.List Projection
  , cursorMap ∷ DimensionMap → Ax.AxisTypeAnnotated s → SM.StrMap s
  , save ∷ DimensionMap → m → Maybe m
  , load ∷ Maybe m → DimensionMap → DimensionMap
  }

type DimensionMap = SM.StrMap D.LabeledJCursor
emptyDimMap ∷ DimensionMap
emptyDimMap = SM.empty

data AnyLens m

packAnyLens ∷ ∀ s m a b. Lens s m a b → AnyLens m
packAnyLens = unsafeCoerce

unpackAnyLens ∷ ∀ s m a b. AnyLens m → Lens s m a b
unpackAnyLens = unsafeCoerce

data AxesProjection

packAxesProjection ∷ ∀ s. (Ax.AxisTypeAnnotated s → s) → AxesProjection
packAxesProjection = unsafeCoerce

unpackAxesProjection ∷ ∀ s. AxesProjection → Ax.AxisTypeAnnotated s → s
unpackAxesProjection = unsafeCoerce

type ProjectionU m a b = At m a b ⇒ Lens' m (Maybe b)
data Projection

packProjection ∷ ∀ m a b. ProjectionU m a b → Projection
packProjection = unsafeCoerce

unpackProjection ∷ ∀ m a b. Projection → ProjectionU m a b
unpackProjection = unsafeCoerce

newtype Field m = Field
  { projection ∷ Projection
  , mandatory ∷ Boolean
  , lens ∷ AnyLens m
  , axesPrjs ∷ L.List AxesProjection
  , guards ∷ L.List Projection
  , filters ∷ L.List Projection
  }

newField ∷ ∀ m. AnyLens m → Projection → Field m
newField lens projection = Field
  { lens
  , projection
  , axesPrjs: L.Nil
  , guards: L.Nil
  , filters: L.Nil
  , mandatory: true
  }

derive instance newtypeField ∷ Newtype (Field m) _

_projection ∷ ∀ m. Lens' (Field m) Projection
_projection = _Newtype ∘ lens _.projection _{ projection = _ }

_mandatory ∷ ∀ m. Lens' (Field m) Boolean
_mandatory = _Newtype ∘ lens _.mandatory _{ mandatory = _ }

_lens ∷ ∀ m. Lens' (Field m) (AnyLens m)
_lens = _Newtype ∘ lens _.lens _{ lens = _ }

_axesPrjs ∷ ∀ m. Lens' (Field m) (L.List AxesProjection)
_axesPrjs = _Newtype ∘ lens _.axesPrjs _{ axesPrjs = _ }

_guards ∷ ∀ m. Lens' (Field m) (L.List Projection)
_guards = _Newtype ∘ lens _.guards _{ guards = _ }

_filters ∷ ∀ m. Lens' (Field m) (L.List Projection)
_filters = _Newtype ∘ lens _.filters _{ filters = _ }

data PackageFF f m next
  = DefineField (AnyLens m) Projection (f → next)
  | Source f AxesProjection (f → next)
  | Depends { filter ∷ f, source ∷ f } (f → next)
  | ActiveGuard { guard ∷ f, source ∷ f } (f → next)
  | ModifyField (f → f) f (f → next)

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
