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

module SlamData.Workspace.Card.Setups.DimMap.Component.Query where

import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.DimensionPicker.Component.Message as DM
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode)
import SlamData.Workspace.Card.Setups.DimMap.Component.State as ST
import SlamData.Workspace.Card.Setups.Package.Types as PT
import SlamData.Workspace.Card.Setups.Transform as Tr

data FieldQuery a
  = Select a
  | Dismiss a
  | Configure a
  | LabelChanged String a
  | HandleDPMessage (DM.Message JCursorNode) a
  | HandleTransformPicker (AS.Message Tr.Transform) a

data Query a
  = OnField PT.Projection (FieldQuery a)
  | Load PT.DimensionMap a
  | Save (PT.DimensionMap → a)
  | SetAxes Ax.Axes a
  | SetPackage ST.Package a

data Message = Update PT.DimensionMap
