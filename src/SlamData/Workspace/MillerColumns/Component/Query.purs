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

module SlamData.Workspace.MillerColumns.Component.Query where

import SlamData.Prelude

import SlamData.Workspace.MillerColumns.Column.Component as Column
import SlamData.Workspace.MillerColumns.Column.Component.Request (LoadRequest, LoadResponse)
import SlamData.Workspace.MillerColumns.Component.State (ColumnsData)
import Halogen.Component.Utils.Drag as Drag
import DOM.Event.Types as DOM

data Query a i o b
  = Populate (ColumnsData a i) b
  | ChangeRoot (ColumnsData a i) b
  | HandleMessage Int i (Column.Message' a i o) b
  | Reload b
  | FulfilLoadRequest (i × LoadResponse a) b
  | DragStart Int DOM.MouseEvent b
  | DragUpdate Int Column.ColumnWidth Drag.DragEvent b

data Message a i
  = SelectionChanged (ColumnsData a i) i (Maybe a)
  | LoadRequest (i × LoadRequest)

type Message' a i o = Either (Message a i) o
