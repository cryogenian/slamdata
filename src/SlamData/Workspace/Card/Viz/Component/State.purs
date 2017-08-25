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

module SlamData.Workspace.Card.Viz.Component.State where

import SlamData.Prelude

import Data.ListMap as LM
import ECharts.Monad (DSL)
import ECharts.Theme (Theme)
import ECharts.Types.Phantom (OptionI)
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.CardType.Chart as Cht
import SlamData.Workspace.Card.Viz.Model as M

type State =
  { dimensions ∷ { width ∷ Int, height ∷ Int }
  , theme ∷ Maybe (Maybe Theme)
  , vizType ∷ Maybe VT.VizType
  , chartOptions ∷ Maybe (DSL OptionI)
  , events ∷ LM.ListMap (Cht.Chart ()) (Array M.FilteredEvent)
  , acceptingEvents ∷ Boolean
  }

initialState ∷ State
initialState =
  { dimensions: { width: 600, height: 400 }
  , vizType: Nothing
  , theme: Nothing
  , chartOptions: Nothing
  , events: LM.empty
  , acceptingEvents: false
  }
