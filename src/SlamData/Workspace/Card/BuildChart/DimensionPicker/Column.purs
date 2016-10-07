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

module SlamData.Workspace.Card.BuildChart.DimensionPicker.Column where

import SlamData.Prelude

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree as Cofree

import Data.Argonaut as J
import Data.List (List(..), (:))
import Data.Map as Map

import SlamData.Workspace.Card.BuildChart.DimensionPicker.Node (discriminateNodes, unNode)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model (Column(..))

type ColumnNode = Either Column Column

groupColumns
  ∷ List Column
  → Cofree List ColumnNode
groupColumns ls =
  discriminateNodes $
    Cofree.mkCofree
      (Column { value: J.JCursorTop, valueAggregation: Nothing })
      (group ls)

  where
  group l =
    fin (foldl go Map.empty l)

  fin m =
    map (uncurry Cofree.mkCofree) (Map.toList (map group m))

  go m col = case col of
    Count →
      Map.insert Count mempty m
    Column { value, valueAggregation } →
      case value of
        J.JField ix J.JCursorTop →
          Map.insert col mempty m
        J.JIndex ix J.JCursorTop →
          Map.insert col mempty m
        J.JField ix next →
          push
            (Column { value: J.JField ix J.JCursorTop, valueAggregation: Nothing })
            (Column { value: next, valueAggregation})
            m
        J.JIndex ix next →
          push
            (Column { value: J.JIndex ix J.JCursorTop, valueAggregation: Nothing })
            (Column { value: next, valueAggregation})
            m
        J.JCursorTop → m

  push k v =
    Map.alter
      case _ of
        Just vs → Just (v : vs)
        Nothing → Just (pure v)
      k

flattenColumns
  ∷ List ColumnNode
  → Column
flattenColumns Nil =
  Column { value: J.JCursorTop, valueAggregation: Nothing }
flattenColumns (c : cs) =
  case unNode c of
    Count → Count
    Column { value } →
      case value of
        J.JField ix _ → mapValue (J.JField ix) (flattenColumns cs)
        J.JIndex ix _ → mapValue (J.JIndex ix) (flattenColumns cs)
        J.JCursorTop → flattenColumns cs
  where
  mapValue f = case _ of
    Count → Count
    Column { value, valueAggregation } →
      Column { value: f value, valueAggregation }
