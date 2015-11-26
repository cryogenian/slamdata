{-
Copyright 2015 SlamData, Inc.

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

module Notebook.Cell.Chart.Component.State where

import Prelude (Unit())
import Halogen (InstalledState())
import Halogen.ECharts (EChartsState(), EChartsQuery())
import Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import Notebook.Common (Slam())

type ChartState =
  { width :: Int
  , height :: Int
  }
type ChartStateP =
  InstalledState ChartState EChartsState CellEvalQuery EChartsQuery Slam Unit

initialState :: ChartState
initialState =
  { width: 600
  , height: 400
  }
