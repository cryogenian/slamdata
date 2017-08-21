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

module SlamData.Workspace.Card.Chart.Component.State where

import SlamData.Prelude
import ECharts.Monad (DSL)
import ECharts.Theme (Theme)
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.CardType.ChartType (ChartType)

-- We shouldn't render ECharts before we determine what theme should be used,
-- because it doesn't allow to change theme, after chart is initialized.
-- For that reason we use double `Maybe` for `theme`
-- * `Nothing` means theme is not initialized,
-- * `Just Nothing` means there is no custom theme.
type State =
  { chartType ∷ Maybe ChartType
  , chartOptions ∷ Maybe (DSL OptionI)
  , theme ∷ Maybe (Maybe Theme)
  , dimensions ∷ { width ∷ Int, height ∷ Int }
  }

initialState :: State
initialState =
  { chartType: Nothing
  , chartOptions: Nothing
  , theme: Nothing
  , dimensions: { width: 600, height: 400 }
  }
