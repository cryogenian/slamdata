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

module SlamData.Workspace.Card.Setups.Chart.Common.Brush where

import SlamData.Prelude

import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types.Phantom as ETP

brush ∷ ∀ i. DSL (toolbox ∷ ETP.I, brush ∷ ETP.I|i)
brush = do
  E.toolbox do
    E.feature do
      E.brushFeature do
        E.brushType do
          E.rect
          E.lineX
          E.lineY
          E.keep
          E.clear

        E.brushTitle do
          E.setRect "Select Rectangle"
          E.setLineX "Select X-Axis"
          E.setLineY "Select Y-Axis"
          E.setKeep "Keep Selection"
          E.setClear "Clear Selection"

  E.brush E.brushModeMultiple
