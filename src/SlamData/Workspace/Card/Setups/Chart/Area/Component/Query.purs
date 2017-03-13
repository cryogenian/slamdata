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

module SlamData.Workspace.Card.Setups.Chart.Area.Component.Query where

import Data.Argonaut (JCursor)

import DOM.Event.Types (Event)

import SlamData.Workspace.Card.Setups.Transform.Aggregation (Aggregation)
import SlamData.Workspace.Card.Setups.Inputs (SelectAction)
import SlamData.Workspace.Card.Setups.DimensionPicker.Component (Message)
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode)

data Selection f
  = Dimension (f JCursor)
  | Value (f JCursor)
  | ValueAgg (f Aggregation)
  | Series (f JCursor)

data Query a
  = SetAxisLabelAngle String a
  | ToggleSmooth a
  | ToggleStacked a
  | Select (Selection SelectAction) a
  | HandleDPMessage (Message JCursorNode) a
  | PreventDefault Event a
