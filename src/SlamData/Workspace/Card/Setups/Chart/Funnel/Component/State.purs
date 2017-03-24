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


module SlamData.Workspace.Card.Setups.Chart.Funnel.Component.State where
{-  ( allFields
  , cursors
  , disabled
  , load
  , save
  , initialState
  , _align
  , _order
  , State
  , module C
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Lens (Traversal', Lens', lens, _Just, (^.), (.~), (^?))
import Data.List as List
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Common.Align (Align(..))
import SlamData.Common.Sort (Sort(..))
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Common.State as C
import SlamData.Workspace.Card.Setups.Dimension as D

type State = C.StateR
  ( align ∷ Align
  , order ∷ Sort
  )

_align ∷ ∀ a r. Lens' { align ∷ a | r } a
_align = lens _.align _{ align = _ }

_order ∷ ∀ a r. Lens' { order ∷ a | r } a
_order = lens _.order _{ order = _ }


allFields ∷ Array C.Projection
allFields =
  [ C._category
  , C._value
  , C._series
  ]

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
    _projection ∷ Traversal' (Maybe D.LabeledJCursor) J.JCursor
    _projection = _Just ∘ D._value ∘ D._projection

    axes = st ^. C._axes

    category =
      axes.category
      ⊕ axes.time
      ⊕ axes.date
      ⊕ axes.datetime

    value =
      axes.value

    series =
      C.mbDelete (st ^? C._dimMap ∘ C.unpack C._category ∘ _projection)
      $ C.ifSelected (st ^? C._dimMap ∘ C.unpack C._category ∘ _projection)
      $ axes.value
      ⊕ axes.time

  in
   SM.fromFoldable
     [ "category" × category
     , "value" × value
     , "series" × series
     ]

initialState ∷ State
initialState =
  { axes: Ax.initialAxes
  , dimMap: SM.empty
  , selected: Nothing

  , align: CenterAlign
  , order: Asc
  }

load ∷ M.AnyCardModel → State → State
load = case _ of
  M.BuildFunnel (Just m) →
    ( C._dimMap ∘ C.unpack C._category .~ Just m.category )
    ∘ ( C._dimMap ∘ C.unpack C._value .~ Just m.value )
    ∘ ( C._dimMap ∘ C.unpack C._series .~ m.series )
    ∘ ( _align .~ m.align )
    ∘ ( _order .~ m.order )
  _ → id

save ∷ State → M.AnyCardModel
save st =
  M.BuildFunnel
  $ { category: _
    , value: _
    , series: st ^. C._dimMap ∘ C.unpack C._series
    , align: st ^. _align
    , order: st ^. _order
    }
  <$> (st ^. C._dimMap ∘ C.unpack C._category)
  <*> (st ^. C._dimMap ∘ C.unpack C._value)
-}
