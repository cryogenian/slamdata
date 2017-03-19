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

module SlamData.Workspace.Card.Setups.Common.Query where

import SlamData.Workspace.Card.Setups.ActionSelect.Component.Message as AS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component.Message (Message)
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.State (Projection)
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode)

data FieldQuery a
  = Select a
  | Dismiss a
  | Configure a
  | LabelChanged String a
  | HandleDPMessage (Message JCursorNode) a
  | HandleTransformPicker (AS.Message T.Transform) a

data QueryR misc a
  = Misc (misc a)
  | OnField Projection (FieldQuery a)
