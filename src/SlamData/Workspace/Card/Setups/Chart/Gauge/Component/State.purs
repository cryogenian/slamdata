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


module SlamData.Workspace.Card.Setups.Chart.Gauge.Component.State
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
import Data.Lens (Traversal', Lens', _Just, (^.), (.~), (^?))
import Data.Lens.At (at)
import Data.List as List
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Common.State as C
import SlamData.Workspace.Card.Setups.Dimension as D

type State = C.StateR ()

allFields ∷ Array C.Projection
allFields =
  [ C.pack (at "value" ∷ ∀ a. Lens' (SM.StrMap a) (Maybe a))
  , C.pack (at "multiple" ∷ ∀ a. Lens' (SM.StrMap a) (Maybe a))
  , C.pack (at "parallel" ∷ ∀ a. Lens' (SM.StrMap a) (Maybe a))
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

    value =
      axes.value

    multiple =
      C.ifSelected (st ^? C._value ∘ _projection)
      $ axes.category
      ⊕ axes.time

    parallel =
      C.mbDelete (st ^? C._multiple ∘ _projection)
      $ C.ifSelected (st ^? C._value ∘ _projection)
      $ axes.category
      ⊕ axes.time

  in
   SM.fromFoldable
     [ "value" × value
     , "multiple" × multiple
     , "parallel" × parallel
     ]

initialState ∷ State
initialState =
  { axes: Ax.initialAxes
  , dimMap: SM.empty
  , selected: Nothing
  }

load ∷ M.AnyCardModel → State → State
load = case _ of
  M.BuildGauge (Just m) →
    ( C._value .~ Just m.value )
    ∘ ( C._multiple .~ m.multiple )
    ∘ ( C._parallel .~ m.parallel )
  _ → id

save ∷ State → M.AnyCardModel
save st =
  M.BuildGauge
  $ { value: _
    , multiple: st ^. C._multiple
    , parallel: st ^. C._parallel
    }
  <$> (st ^. C._value)
