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

module SlamData.Workspace.Card.Setups.Chart.Area.Component.State where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Lens (Traversal', Lens', _Just, lens, (^.), (.~), (^?))
import Data.Lens.At (at)
import Data.List as List
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

import Unsafe.Coerce (unsafeCoerce)

-- TODO: move it somewhere, it should live in different place with group/flatten
showJCursor ∷ J.JCursor → String
showJCursor (J.JField i c) = i <> show c
showJCursor J.JCursorTop = "value"
showJCursor c = show c

type DimensionMap = SM.StrMap D.LabeledJCursor

type ProjectionU a = Lens' (SM.StrMap a) (Maybe a)
data Projection

pack ∷ ∀ a. ProjectionU a → Projection
pack = unsafeCoerce

unpack ∷ ∀ a. Projection → ProjectionU a
unpack = unsafeCoerce

-- TODO: this should be hidden. We rely on its internal structure too often
type State =
  { axes ∷ Ax.Axes
  , dimMap ∷ DimensionMap
  , selected ∷ Maybe (Projection ⊹ Projection)

  , isSmooth ∷ Boolean
  , isStacked ∷ Boolean
  , axisLabelAngle ∷ Number
  }

_axes ∷ ∀ a r. Lens' { axes ∷ a | r } a
_axes = lens _.axes _{ axes = _ }

_selected ∷ ∀ a r. Lens' { selected ∷ a | r } a
_selected = lens _.selected _{ selected = _ }

_dimMap ∷ ∀ a r. Lens' { dimMap ∷ a | r } a
_dimMap = lens _.dimMap _{ dimMap = _ }

_dimension ∷ Lens' State (Maybe D.LabeledJCursor)
_dimension = _dimMap ∘ at "dimension"

_value ∷ Lens' State (Maybe D.LabeledJCursor)
_value = _dimMap ∘ at "value"

_series ∷ Lens' State (Maybe D.LabeledJCursor)
_series = _dimMap ∘ at "series"

_isSmooth ∷ Lens' State Boolean
_isSmooth = lens _.isSmooth _{ isSmooth = _ }

_isStacked ∷ Lens' State Boolean
_isStacked = lens _.isStacked _{ isStacked = _ }

_axisLabelAngle ∷ Lens' State Number
_axisLabelAngle = lens _.axisLabelAngle _{ axisLabelAngle = _ }


allFields ∷ Array Projection
allFields =
  [ pack (at "dimension" ∷ ∀ a. Lens' (SM.StrMap a) (Maybe a))
  , pack (at "value" ∷ ∀ a. Lens' (SM.StrMap a) (Maybe a))
  , pack (at "series" ∷ ∀ a. Lens' (SM.StrMap a) (Maybe a))
  ]

initialState ∷ State
initialState =
  { axes: Ax.initialAxes
  , dimMap: SM.empty
  , selected: Nothing

  , isSmooth: false
  , isStacked: false
  , axisLabelAngle: 0.0
  }

cursors ∷ State → List.List J.JCursor
cursors st = case st.selected of
  Just (Left lns) → List.fromFoldable $ fldCursors lns st
  _ → List.Nil

disabled ∷ Projection → State → Boolean
disabled fld st = Set.isEmpty $ fldCursors fld st

fldCursors ∷ Projection → State → Set.Set J.JCursor
fldCursors fld st =
  fromMaybe Set.empty $ cursorMap st ^. unpack fld

cursorMap ∷ State → SM.StrMap (Set.Set J.JCursor)
cursorMap st =
  let
    mbDelete ∷ ∀ a. Ord a ⇒ Maybe a → Set.Set a → Set.Set a
    mbDelete mbA s = maybe s (flip Set.delete s) mbA

    _projection ∷ Traversal' (Maybe D.LabeledJCursor) J.JCursor
    _projection = _Just ∘ D._value ∘ D._projection

    axes = st ^. _axes

    dimension =
      axes.category
      ⊕ axes.time
      ⊕ axes.value
      ⊕ axes.date
      ⊕ axes.datetime

    value =
      mbDelete (st ^? _dimension ∘ _projection)
      $ axes.value

    series =
      mbDelete (st ^? _dimension ∘ _projection)
      $ mbDelete (st ^? _value ∘ _projection)
      $ axes.value

  in
   SM.fromFoldable
     [ "dimension" × dimension
     , "value" × value
     , "series" × series
     ]

transforms ∷ State → Array T.Transform
transforms _ = T.aggregationTransforms

load ∷ M.AnyCardModel → State → State
load = case _ of
  M.BuildArea (Just m) →
    ( _dimension .~ Just m.dimension )
    ∘ ( _value .~ Just m.value )
    ∘ ( _series .~ m.series )
    ∘ ( _isStacked .~ m.isStacked )
    ∘ ( _isSmooth .~ m.isSmooth )
    ∘ ( _axisLabelAngle .~ m.axisLabelAngle )
  _ → id

save ∷ State → M.AnyCardModel
save st =
  M.BuildArea
  $ { dimension: _
    , value: _
    , series: st ^. _series
    , isStacked: st ^. _isStacked
    , isSmooth: st ^. _isSmooth
    , axisLabelAngle: st ^. _axisLabelAngle
    }
  <$> (st ^. _dimension)
  <*> (st ^. _value)

setValue ∷ Projection → J.JCursor → State → State
setValue fld v =
  _dimMap ∘ unpack fld .~ (wrapFn <*> pure v)
  where
  wrapFn = wrapFns ^. unpack fld
  wrapFns = SM.fromFoldable
    [ "dimension" × D.projection
    , "value" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "high" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "low" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "open" × (D.projectionWithAggregation $ Just Ag.Sum)
    , "close" × D.projection
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
    , "measure" × "Measure label"
    ]

setTransform ∷ Projection → Maybe T.Transform → State → State
setTransform fld t =
  _dimMap ∘ unpack fld ∘ _Just ∘ D._value ∘ D._transform .~ t

setLabel ∷ Projection → String → State → State
setLabel fld str =
  _dimMap ∘ unpack fld ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str

clear ∷ Projection → State → State
clear fld =
  _dimMap ∘ unpack fld .~ Nothing

select ∷ Projection → State → State
select fld =
  _selected .~ (Just $ Left fld)

configure ∷ Projection → State → State
configure fld =
  _selected .~ (Just $ Right fld)

deselect ∷ State → State
deselect =
  _selected .~ Nothing

getSelected ∷ Projection → State → Maybe D.LabeledJCursor
getSelected fld state = state ^. _dimMap ∘ unpack fld
