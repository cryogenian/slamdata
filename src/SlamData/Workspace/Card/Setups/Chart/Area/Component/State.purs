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

module SlamData.Workspace.Card.Setups.Chart.Area.Component.State
  ( allFields
  , cursors
  , disabled
  , load
  , save
  , initialState
  , State
  , module C
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Lens (Traversal', Lens', lens, _Just, (^.), (.~), (^?))
import Data.Lens.At (at)
import Data.List as List
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Common.State as C
import SlamData.Workspace.Card.Setups.Dimension as D

type State = C.StateR
  ( isSmooth ∷ Boolean
  , isStacked ∷ Boolean
  , axisLabelAngle ∷ Number
  )

allFields ∷ Array C.Projection
allFields =
  [ C.pack (at "dimension" ∷ ∀ a. Lens' (SM.StrMap a) (Maybe a))
  , C.pack (at "value" ∷ ∀ a. Lens' (SM.StrMap a) (Maybe a))
  , C.pack (at "series" ∷ ∀ a. Lens' (SM.StrMap a) (Maybe a))
  ]

_isSmooth ∷ Lens' State Boolean
_isSmooth = lens _.isSmooth _{ isSmooth = _ }

_isStacked ∷ Lens' State Boolean
_isStacked = lens _.isStacked _{ isStacked = _ }

_axisLabelAngle ∷ Lens' State Number
_axisLabelAngle = lens _.axisLabelAngle _{ axisLabelAngle = _ }

cursors ∷ State → List.List J.JCursor
cursors st = case st.selected of
  Just (Left lns) → List.fromFoldable $ fldCursors lns st
  _ → List.Nil

disabled ∷ C.Projection → State → Boolean
disabled fld st = Set.isEmpty $ fldCursors fld st

fldCursors ∷ C.Projection → State → Set.Set J.JCursor
fldCursors fld st =
  fromMaybe Set.empty $ cursorMap st ^. C.unpack fld

cursorMap ∷ State → SM.StrMap (Set.Set J.JCursor)
cursorMap st =
  let
    mbDelete ∷ ∀ a. Ord a ⇒ Maybe a → Set.Set a → Set.Set a
    mbDelete mbA s = maybe s (flip Set.delete s) mbA

    _projection ∷ Traversal' (Maybe D.LabeledJCursor) J.JCursor
    _projection = _Just ∘ D._value ∘ D._projection

    axes = st ^. C._axes

    dimension =
      axes.category
      ⊕ axes.time
      ⊕ axes.value
      ⊕ axes.date
      ⊕ axes.datetime

    value =
      mbDelete (st ^? C._dimension ∘ _projection)
      $ axes.value

    series =
      mbDelete (st ^? C._dimension ∘ _projection)
      $ mbDelete (st ^? C._value ∘ _projection)
      $ axes.value

  in
   SM.fromFoldable
     [ "dimension" × dimension
     , "value" × value
     , "series" × series
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

load ∷ M.AnyCardModel → State → State
load = case _ of
  M.BuildArea (Just m) →
    ( C._dimension .~ Just m.dimension )
    ∘ ( C._value .~ Just m.value )
    ∘ ( C._series .~ m.series )
    ∘ ( _isStacked .~ m.isStacked )
    ∘ ( _isSmooth .~ m.isSmooth )
    ∘ ( _axisLabelAngle .~ m.axisLabelAngle )
  _ → id

save ∷ State → M.AnyCardModel
save st =
  M.BuildArea
  $ { dimension: _
    , value: _
    , series: st ^. C._series
    , isStacked: st ^. _isStacked
    , isSmooth: st ^. _isSmooth
    , axisLabelAngle: st ^. _axisLabelAngle
    }
  <$> (st ^. C._dimension)
  <*> (st ^. C._value)
