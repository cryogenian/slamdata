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

module Notebook.Cell.Viz.Component.Query
  ( VizQuery(..)
  , VizQueryP()
  ) where

import Data.Functor.Coproduct (Coproduct())
import Halogen
import Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import Notebook.Cell.Viz.Form.Component as Form
import Data.Set (Set())
import Model.ChartType (ChartType())

data VizQuery a
  = SetHeight Int a
  | SetWidth Int a
  | SetAvailableChartTypes (Set ChartType) a
  | SetChartType ChartType a
  | RotateAxisLabel Int a
  | SetAxisFontSize Int a

type VizQueryP =
  Coproduct
  (Coproduct CellEvalQuery VizQuery)
  (ChildF ChartType Form.QueryP)
