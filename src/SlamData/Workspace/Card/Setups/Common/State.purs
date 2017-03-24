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
import Data.Set as Set
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
emptyDimMap ∷ DimensionMap
emptyDimMap = SM.empty

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

_open ∷ Projection
_open = pack $ at "open"

_close ∷ Projection
_close = pack $ at "close"

_high ∷ Projection
_high = pack $ at "high"

_low ∷ Projection
_low = pack $ at "low"

_dimension ∷ Projection
_dimension = pack $ at "dimension"

_parallel ∷ Projection
_parallel = pack $ at "parallel"

_value ∷ Projection
_value = pack $ at "value"

_series ∷ Projection
_series = pack $ at "series"

_category ∷ Projection
_category = pack $ at "category"

_stack ∷ Projection
_stack = pack $ at "stack"

_multiple ∷ Projection
_multiple = pack $ at "multiple"

_source ∷ Projection
_source = pack $ at "source"

_target ∷ Projection
_target = pack $ at "target"

_abscissa ∷ Projection
_abscissa = pack $ at "abscissa"

_ordinate ∷ Projection
_ordinate = pack $ at "ordinate"

_secondValue ∷ Projection
_secondValue = pack $ at "secondValue"

_donut ∷ Projection
_donut = pack $ at "donut"

-- Encode integer indices of List LabeledJCursor as Strings
_dimIx ∷ Int → Projection
_dimIx i = pack $ at (show i)

printProjection ∷ Projection → Maybe String
printProjection prj = idReflection ^. unpack prj

-- Determine if this is integer index :)
mbDimIx ∷ Projection → Maybe Int
mbDimIx fld = Int.fromString =<< idReflection ^. unpack fld

_dims ∷ ∀ a. Lens' (SM.StrMap a) (L.List a)
_dims = lens get set
  where
  get ∷ SM.StrMap a → L.List a
  get sm =
    foldMap (\(k × v) → maybe L.Nil (const $ L.singleton v) $ Int.fromString k) $ SM.toList sm

  set ∷ SM.StrMap a → L.List a → SM.StrMap a
  set sm lst = flip SM.union sm $ SM.fromFoldable $ flip L.mapWithIndex lst \v i → show i × v

transforms ∷ ∀ r. StateR r → Array T.Transform
transforms _ = T.aggregationTransforms

setValue ∷ ∀ r. Projection → J.JCursor → StateR r → StateR r
setValue fld v =
  _dimMap ∘ unpack fld ?~ wrapFn v
  where
  wrapFn = fromMaybe D.projection $ wrapFns ^. unpack fld
  wrapFns =
    SM.empty
      # (unpack _dimension ?~ D.projection)
      ∘ (unpack _high ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (unpack _low ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (unpack _open ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (unpack _close ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (unpack _parallel ?~ D.projection)
      ∘ (unpack _value ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (unpack _series ?~ D.projection)
      ∘ (unpack _category ?~ D.projection)
      ∘ (unpack _stack ?~ D.projection)
      ∘ (unpack _source ?~ D.projection)
      ∘ (unpack _target ?~ D.projection)
      ∘ (unpack _abscissa ?~ D.projection)
      ∘ (unpack _ordinate ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (unpack _secondValue ?~ (D.projectionWithAggregation $ Just Ag.Sum))
      ∘ (unpack _donut ?~ D.projection)

showValue ∷ Projection → Maybe J.JCursor → String
showValue fld c = do
  fromMaybe "" $ map showJCursor c <|> (values ^. unpack fld)
  where
  values =
    SM.empty
      # (unpack _dimension ?~ "Select dimension")
      ∘ (unpack _high ?~ "Select high")
      ∘ (unpack _low ?~ "Select low")
      ∘ (unpack _open ?~ "Select open")
      ∘ (unpack _close ?~ "Select close")
      ∘ (unpack _parallel ?~ "Select parallel")
      ∘ (unpack _value ?~ "Select measure")
      ∘ (unpack _series ?~ "Select series")
      ∘ (unpack _category ?~ "Select category")
      ∘ (unpack _stack ?~ "Select stack")
      ∘ (unpack _source ?~ "Select source")
      ∘ (unpack _target ?~ "Select target")
      ∘ (unpack _abscissa ?~ "Select X-Axis")
      ∘ (unpack _ordinate ?~ "Select Y-Axis")
      ∘ (unpack _secondValue ?~ "Select the second measure")
      ∘ (unpack _donut ?~ "Select donut")

chooseLabel ∷ Projection → String
chooseLabel fld = fromMaybe "" $ labels ^. unpack fld
  where
  labels =
    SM.empty
      # (unpack _dimension ?~ "Choose dimension")
      ∘ (unpack _high ?~ "Choose measure for high position")
      ∘ (unpack _low ?~ "Choose measure for close position")
      ∘ (unpack _open ?~ "Choose measure for open position")
      ∘ (unpack _close ?~ "Choose measure for close position")
      ∘ (unpack _parallel ?~ "Choose measure for low position")
      ∘ (unpack _value ?~ "Choose measure")
      ∘ (unpack _series ?~ "Choose series")
      ∘ (unpack _category ?~ "Choose category")
      ∘ (unpack _stack ?~ "Choose stack")
      ∘ (unpack _source ?~ "Choose source")
      ∘ (unpack _target ?~ "Choose target")
      ∘ (unpack _abscissa ?~ "Choose X-Axis")
      ∘ (unpack _ordinate ?~ "Choose Y-Axis")
      ∘ (unpack _secondValue ?~ "Choose the second measure")
      ∘ (unpack _donut ?~ "Choose donut")


showDefaultLabel ∷ Projection → Maybe J.JCursor → String
showDefaultLabel fld c =
  fromMaybe "" $ (labels ^. unpack fld) <|> map showJCursor c
  where
  labels =
    SM.empty
      # (unpack _dimension ?~ "Dimension label")
      ∘ (unpack _high ?~ "High position label")
      ∘ (unpack _low ?~ "Low position label")
      ∘ (unpack _open ?~ "Open position label")
      ∘ (unpack _close ?~ "Close position label")
      ∘ (unpack _parallel ?~ "Parallel label")
      ∘ (unpack _value ?~ "Measure label")
      ∘ (unpack _series ?~ "Series label")
      ∘ (unpack _category ?~ "Category label")
      ∘ (unpack _stack ?~ "Stack label")
      ∘ (unpack _source ?~ "Source label")
      ∘ (unpack _target ?~ "Target label")
      ∘ (unpack _abscissa ?~ "X-Axis label")
      ∘ (unpack _ordinate ?~ "Y-Axis label")
      ∘ (unpack _secondValue ?~ "Measure#2 label")
      ∘ (unpack _donut ?~ "Donut label")

setTransform ∷ ∀ r. Projection → Maybe T.Transform → StateR r → StateR r
setTransform fld t =
  _dimMap ∘ unpack fld ∘ _Just ∘ D._value ∘ D._transform .~ t

setLabel ∷ ∀ r. Projection → String → StateR r → StateR r
setLabel fld str =
  _dimMap ∘ unpack fld ∘ _Just ∘ D._category ?~ D.Static str

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

mbDelete ∷ ∀ a. Ord a ⇒ Maybe a → Set.Set a → Set.Set a
mbDelete mb s = maybe s (flip Set.delete s) mb

ifSelected ∷ ∀ a. Ord a ⇒ Maybe a → Set.Set a → Set.Set a
ifSelected mb s = case mb of
  Nothing → Set.empty
  _ → s
