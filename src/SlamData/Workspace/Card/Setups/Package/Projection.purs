module SlamData.Workspace.Card.Setups.Package.Projection where

import SlamData.Prelude

import Data.Int as Int
import Data.Lens (Lens', lens, (^.))
import Data.Lens.At (at)
import Data.List as L
import Data.StrMap as SM

import SlamData.Workspace.Card.Setups.Package.Types as T

_open ∷ T.Projection
_open = T.packProjection $ at "open"

_close ∷ T.Projection
_close = T.packProjection $ at "close"

_high ∷ T.Projection
_high = T.packProjection $ at "high"

_low ∷ T.Projection
_low = T.packProjection $ at "low"

_dimension ∷ T.Projection
_dimension = T.packProjection $ at "dimension"

_parallel ∷ T.Projection
_parallel = T.packProjection $ at "parallel"

_value ∷ T.Projection
_value = T.packProjection $ at "value"

_series ∷ T.Projection
_series = T.packProjection $ at "series"

_category ∷ T.Projection
_category = T.packProjection $ at "category"

_stack ∷ T.Projection
_stack = T.packProjection $ at "stack"

_multiple ∷ T.Projection
_multiple = T.packProjection $ at "multiple"

_source ∷ T.Projection
_source = T.packProjection $ at "source"

_target ∷ T.Projection
_target = T.packProjection $ at "target"

_abscissa ∷ T.Projection
_abscissa = T.packProjection $ at "abscissa"

_ordinate ∷ T.Projection
_ordinate = T.packProjection $ at "ordinate"

_secondValue ∷ T.Projection
_secondValue = T.packProjection $ at "secondValue"

_donut ∷ T.Projection
_donut = T.packProjection $ at "donut"

-- Encode integer indices of List LabeledJCursor as Strings
_dimIx ∷ Int → T.Projection
_dimIx i = T.packProjection $ at (show i)

printProjection ∷ T.Projection → Maybe String
printProjection prj = T.idReflection ^. T.unpackProjection prj

-- Determine if this is integer index :)
mbDimIx ∷ T.Projection → Maybe Int
mbDimIx fld = Int.fromString =<< T.idReflection ^. T.unpackProjection fld

_dims ∷ ∀ a. Lens' (SM.StrMap a) (L.List a)
_dims = lens get set
  where
  get ∷ SM.StrMap a → L.List a
  get sm =
    foldMap (\(k × v) → maybe L.Nil (const $ L.singleton v) $ Int.fromString k) $ SM.toList sm

  set ∷ SM.StrMap a → L.List a → SM.StrMap a
  set sm lst = flip SM.union sm $ SM.fromFoldable $ flip L.mapWithIndex lst \v i → show i × v
