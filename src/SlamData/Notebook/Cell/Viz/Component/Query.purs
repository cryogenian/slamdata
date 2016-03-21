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

module SlamData.Notebook.Cell.Viz.Component.Query where

import Data.Functor.Coproduct (Coproduct)
import Data.Set (Set)

import Halogen (ChildF)

import SlamData.Notebook.Cell.Chart.ChartType (ChartType)
import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery)
import SlamData.Notebook.Cell.Viz.Form.Component as Form

data Query a
  = SetHeight Int a
  | SetWidth Int a
  | SetAvailableChartTypes (Set ChartType) a
  | SetChartType ChartType a
  | RotateAxisLabel Int a
  | SetAxisFontSize Int a

type QueryC = Coproduct CellEvalQuery Query

type QueryP = Coproduct QueryC (ChildF ChartType Form.QueryP)
