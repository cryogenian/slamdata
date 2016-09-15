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

module SlamData.Workspace.Card.BuildChart.PivotTable.Component.State 
  ( State
  , StateP
  , initialState
  , modelFromState
  , stateFromModel
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.PivotTable.Component.ChildSlot as PCS
import SlamData.Workspace.Card.BuildChart.PivotTable.Component.Query (QueryC)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model (Model, Column)
import SlamData.Workspace.Card.Chart.Axis (Axes, initialAxes)

type StateP =
  ParentState State PCS.ChildState QueryC PCS.ChildQuery Slam PCS.ChildSlot

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , fresh ∷ Int
  , dimensions ∷ Map Int JCursor
  , columns ∷ Map Int Column
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , fresh: 0
  , dimensions: Map.empty
  , columns: Map.empty
  }

modelFromState ∷ State → Model
modelFromState st = Just
  { dimensions: Array.fromFoldable st.dimensions
  , columns: Array.fromFoldable st.columns
  }

stateFromModel ∷ Model → State → State
stateFromModel Nothing st = st { dimensions = Map.empty, columns = Map.empty, fresh = 0 }
stateFromModel (Just r) st =
  let
    l1 = Array.length r.dimensions
    l2 = Array.length r.columns
    dims = Map.fromFoldable (Array.mapWithIndex Tuple r.dimensions)
    cols = Map.fromFoldable (Array.mapWithIndex (\i → Tuple (i + l1)) r.columns)
    fresh = l1 + l2
  in
    st { dimensions = dims, columns = cols, fresh = fresh }
