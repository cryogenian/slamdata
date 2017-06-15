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

module SlamData.Workspace.Card.Setups.Package.Projection where

import SlamData.Prelude

import Data.Int as Int
import Data.Lens (Lens', lens, (^.))
import Data.Lens.At (at)
import Data.List as L
import Data.StrMap as SM

import SlamData.Workspace.Card.Setups.Package.Types as T

_flatValue ∷ T.Projection
_flatValue = T.packProjection $ at "flatValue"

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

_size ∷ T.Projection
_size = T.packProjection $ at "size"

_color ∷ T.Projection
_color = T.packProjection $ at "color"

_scatterOrdinate ∷ T.Projection
_scatterOrdinate = T.packProjection $ at "scatterOrdinate"

_scatterSize ∷ T.Projection
_scatterSize = T.packProjection $ at "scatterSize"

_lat ∷ T.Projection
_lat = T.packProjection $ at "lat"

_lng ∷ T.Projection
_lng = T.packProjection $ at "lng"

_intensity ∷ T.Projection
_intensity = T.packProjection $ at "intensity"

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
    foldMap (\(k × v) → maybe L.Nil (const $ L.singleton v) $ Int.fromString k)
      $ asList
      $ SM.toUnfoldable sm

  set ∷ SM.StrMap a → L.List a → SM.StrMap a
  set sm lst = flip SM.union sm $ SM.fromFoldable $ flip L.mapWithIndex lst \i v → show i × v

_formValue ∷ T.Projection
_formValue = T.packProjection $ at "formValue"

_formLabel ∷ T.Projection
_formLabel = T.packProjection $ at "formLabel"

_formSelected ∷ T.Projection
_formSelected = T.packProjection $ at "formSelected"
