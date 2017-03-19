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

module SlamData.Workspace.Card.Setups.Common.State where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Int as Int
import Data.Lens (wander, Lens', _Just, lens, (^.), (.~), (?~))
import Data.Lens.At (class At, at)
import Data.Lens.Index (class Index)
import Data.List as L
import Data.StrMap as SM

import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

import Unsafe.Coerce (unsafeCoerce)

-- TODO: move it somewhere, it should live in different place with group/flatten
showJCursor ∷ J.JCursor → String
showJCursor = case _ of
  J.JCursorTop → "value"
  J.JField i c → i ⊕ show c
  c → show c

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

type StateR r =
  { axes ∷ Ax.Axes
  , dimMap ∷ DimensionMap
  , selected ∷ Maybe (Projection ⊹ Projection)
  | r
  }

type DimensionMap = SM.StrMap D.LabeledJCursor

type ProjectionU m a b = At m a b ⇒ Lens' m (Maybe b)
data Projection

pack ∷ ∀ m a b. ProjectionU m a b → Projection
pack = unsafeCoerce

unpack ∷ ∀ m a b. Projection → ProjectionU m a b
unpack = unsafeCoerce

_axes ∷ ∀ a r. Lens' { axes ∷ a | r } a
_axes = lens _.axes _{ axes = _ }

_selected ∷ ∀ a r. Lens' { selected ∷ a | r } a
_selected = lens _.selected _{ selected = _ }

_dimMap ∷ ∀ a r. Lens' { dimMap ∷ a | r } a
_dimMap = lens _.dimMap _{ dimMap = _ }

_open ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_open = _dimMap ∘ at "open"

_close ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_close = _dimMap ∘ at "close"

_high ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_high = _dimMap ∘ at "high"

_low ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_low = _dimMap ∘ at "low"

_dimension ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_dimension = _dimMap ∘ at "dimension"

_parallel ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_parallel = _dimMap ∘ at "parallel"

_value ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_value = _dimMap ∘ at "value"

_series ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_series = _dimMap ∘ at "series"

_category ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_category = _dimMap ∘ at "category"

_stack ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_stack = _dimMap ∘ at "stack"

_multiple ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_multiple = _dimMap ∘ at "multiple"

_source ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_source = _dimMap ∘ at "source"

_target ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_target = _dimMap ∘ at "target"

_abscissa ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_abscissa = _dimMap ∘ at "abscissa"

_ordinate ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_ordinate = _dimMap ∘ at "ordinate"

_secondValue ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_secondValue = _dimMap ∘ at "secondValue"

_donut ∷ ∀ r. Lens' (StateR r) (Maybe D.LabeledJCursor)
_donut = _dimMap ∘ at "donut"

-- Encode integer indices of List LabeledJCursor as Strings
_dimIx ∷ ∀ r. Int → Lens' (StateR r) (Maybe D.LabeledJCursor)
_dimIx i = _dimMap ∘ at (show i)

-- Determine if this is integer index :)
mbDimIx ∷ Projection → Maybe Int
mbDimIx fld = Int.fromString =<< idReflection ^. unpack fld

_dims ∷ ∀ r. Lens' (StateR r) (L.List D.LabeledJCursor)
_dims = _dimMap ∘ lens get set
  where
  get ∷ DimensionMap → L.List D.LabeledJCursor
  get sm =
    foldMap (\(k × v) → maybe L.Nil (const $ L.singleton v) $ Int.fromString k) $ SM.toList sm

  set ∷ DimensionMap → L.List D.LabeledJCursor → DimensionMap
  set sm lst = flip SM.union sm $ SM.fromFoldable $ flip L.mapWithIndex lst \v i → show i × v

transforms ∷ ∀ r. StateR r → Array T.Transform
transforms _ = T.aggregationTransforms

-- TODO: encode all set/show/etc for dimIx precisely
setValue ∷ ∀ r. Projection → J.JCursor → StateR r → StateR r
setValue fld v =
  _dimMap ∘ unpack fld ?~ wrapFn v
  where
  wrapFn = fromMaybe D.projection $ wrapFns ^. unpack fld
  wrapFns = SM.fromFoldable
    [ "dimension" × D.projection
    , "high" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "low" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "open" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "close" × D.projection
    , "parallel" × D.projection
    , "value" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "series" × D.projection
    , "category" × D.projection
    , "stack" × D.projection
    , "source" × D.projection
    , "target" × D.projection
    , "abscissa" × D.projection
    , "ordinate" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "secondValue" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "donut" × D.projection
    ]

showValue ∷ Projection → Maybe J.JCursor → String
showValue fld c = do
  fromMaybe "" $ (values ^. unpack fld) <|> map showJCursor c
  where
  values = SM.fromFoldable
    [ "dimension" × "Select dimension"
    , "open" × "Select open"
    , "close" × "Select close"
    , "high" × "Select high"
    , "low" × "Select low"
    , "parallel" × "Select parallel"
    , "value" × "Select measure"
    , "series" × "Select series"
    , "category" × "Select category"
    , "stack" × "Select stack"
    , "source" × "Select source"
    , "target" × "Select target"
    , "abscissa" × "Select X-axis"
    , "ordinate" × "Select Y-axis"
    , "secondValue" × "Select the second measure"
    , "donut" × "Select donut"
    ]

chooseLabel ∷ Projection → String
chooseLabel fld = fromMaybe "" $ labels ^. unpack fld
  where
  labels = SM.fromFoldable
    [ "dimension" × "Choose dimension"
    , "open" × "Choose measure for open position"
    , "close" × "Choose measure for close position"
    , "high" × "Choose measure for high position"
    , "low" × "Choose measure for low position"
    , "parallel" × "Choose parallel"
    , "value" × "Choose measure"
    , "series" × "Choose series"
    , "category" × "Choose category"
    , "stack" × "Choose stack"
    , "source" × "Choose source"
    , "target" × "Choose target"
    , "abscissa" × "Choose X-axis"
    , "ordinate" × "Choose Y-axis"
    , "secondValue" × "Choose the second measure"
    , "donut" × "Choose donut"
    ]

showDefaultLabel ∷ Projection → Maybe J.JCursor → String
showDefaultLabel fld c =
  fromMaybe "" $ (labels ^. unpack fld) <|> map showJCursor c
  where
  labels = SM.fromFoldable
    [ "dimension" × "Dimension label"
    , "open" × "Open position label"
    , "close" × "Close position label"
    , "high" × "High position label"
    , "low" ×  "Low position label"
    , "parallel" × "Parallel label"
    , "value" × "Measure label"
    , "series" × "Series label"
    , "category" × "Category label"
    , "stack" × "Stack label"
    , "source" × "Source label"
    , "target" × "Target label"
    , "abscissa" × "X-axis label"
    , "ordinate" × "Y-axis label"
    , "secondValue" × "Measure#2 label"
    , "donut" × "Donut label"
    ]

setTransform ∷ ∀ r. Projection → Maybe T.Transform → StateR r → StateR r
setTransform fld t =
  _dimMap ∘ unpack fld ∘ _Just ∘ D._value ∘ D._transform .~ t

setLabel ∷ ∀ r. Projection → String → StateR r → StateR r
setLabel fld str =
  _dimMap ∘ unpack fld ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str

clear ∷ ∀ r. Projection → StateR r → StateR r
clear fld =
  _dimMap ∘ unpack fld .~ Nothing

select ∷ ∀ r. Projection → StateR r → StateR r
select fld =
  _selected .~ (Just $ Left fld)

configure ∷ ∀ r. Projection → StateR r → StateR r
configure fld =
  _selected .~ (Just $ Right fld)

deselect ∷ ∀ r. StateR r → StateR r
deselect =
  _selected .~ Nothing

getSelected ∷ ∀ r. Projection → StateR r → Maybe D.LabeledJCursor
getSelected fld state = state ^. _dimMap ∘ unpack fld
