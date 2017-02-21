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

module SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.Query where

import SlamData.Prelude

import Halogen.Component.Utils.Drag (DragEvent)

import SlamData.Workspace.Card.Setups.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode)
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (ColumnNode)
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DP

import Utils.DOM as DOM

data Query a
  = AddDimension a
  | RemoveDimension Int a
  | AddColumn a
  | RemoveColumn Int a
  | OrderDimensionStart Int DOM.MouseEvent a
  | OrderingDimension Int DragEvent a
  | OrderOverDimension Int a
  | OrderOutDimension Int a
  | OrderColumnStart Int DOM.MouseEvent a
  | OrderingColumn Int DragEvent a
  | OrderOverColumn Int a
  | OrderOutColumn Int a
  | ChooseAggregation Int (Maybe Ag.Aggregation) a
  | HandleDimPicker (DP.Message JCursorNode) a
  | HandleColPicker (DP.Message ColumnNode) a
