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

import Data.Lens (Lens', lens)

import SlamData.Common.Align (Align(..))
import SlamData.Common.Sort (Sort(..))

type State =
  { order ∷ Sort
  , align ∷ Align
  }

initialState ∷ State
initialState =
  { order: Asc
  , align: LeftAlign
  }

_order ∷ Lens' State Sort
_order = lens _.order _{ order = _ }

_align ∷ Lens' State Align
_align = lens _.align _{ align = _ }
