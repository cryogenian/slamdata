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

module SlamData.Workspace.Card.Setups.DimensionPicker.Column where

import SlamData.Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree as CF

import Data.Argonaut as J
import Data.List ((:))
import Data.List as L

import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model (Column(..))
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (showJCursor, unfoldJCursor)
import SlamData.Workspace.Card.Setups.DimensionPicker.Node (discriminateNodes)
import SlamData.Workspace.MillerColumns.TreeData (constructTree)

type ColumnNode = Either Column Column

groupColumns ∷ L.List Column → CF.Cofree L.List ColumnNode
groupColumns cs =
  let
    root = Column { value: J.JCursorTop, valueAggregation: Nothing }
    tree = constructTree unfoldColumn root cs
  in
    discriminateNodes
      if extract <$> CF.tail tree == pure Count
      then CF.mkCofree root (CF.mkCofree Count L.Nil : CF.mkCofree root L.Nil : L.Nil)
      else tree

unfoldColumn :: Column → Maybe (Tuple Column Column)
unfoldColumn col = case col of
  Count → Nothing
  Column { value, valueAggregation } →
    unfoldJCursor value <#> \(Tuple cur rest) →
      Tuple col (Column { value: rest, valueAggregation: Nothing })

showColumn ∷ Column → String
showColumn (Column { value }) = showJCursor value
showColumn Count = "COUNT"

flattenColumns ∷ ColumnNode → Column
flattenColumns = either id id
