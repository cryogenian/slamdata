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

module SlamData.Workspace.Card.Setups.Chart.Graph.Component.State
  ( initialState
  , State
  , _source
  , _target
  , _size
  , _sizeAgg
  , _color
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Graph.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Graph.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , maxSize: M.initialState.maxSize
  , minSize: M.initialState.minSize
  , circular: M.initialState.circular
  , source: M.initialState.source
  , target: M.initialState.target
  , size: M.initialState.size
  , sizeAgg: M.initialState.sizeAgg
  , color: M.initialState.color
  , picker: DS.initial.picker
  }

_source ∷ ∀ r a. Lens' { source ∷ a | r } a
_source = lens _.source _{ source = _ }

_target ∷ ∀ r a. Lens' { target ∷ a | r } a
_target = lens _.target _{ target = _ }

_size ∷ ∀ r a. Lens' { size ∷ a | r } a
_size = lens _.size _{ size = _ }

_sizeAgg ∷ ∀ r a. Lens' { sizeAgg ∷ a | r } a
_sizeAgg = lens _.sizeAgg _{ sizeAgg = _ }

_color ∷ ∀ r a. Lens' { color ∷ a | r } a
_color = lens _.color _{ color = _ }
