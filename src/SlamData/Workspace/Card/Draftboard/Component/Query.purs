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

module SlamData.Workspace.Card.Draftboard.Component.Query where

import SlamData.Prelude
import Halogen.Component.Utils.Drag (DragEvent)
import SlamData.Workspace.Card.Draftboard.Layout (SplitBias, Edge)
import SlamData.Workspace.Card.Draftboard.Orientation (Orientation)
import SlamData.Workspace.Card.Draftboard.Pane (Cursor)
import SlamData.Workspace.Deck.DeckId (DeckId)
import Utils.DOM as DOM

data Query a
  = SplitStart Orientation SplitBias Boolean DOM.MouseEvent a
  | Splitting DragEvent a
  | ResizeStart (Edge Number) DOM.MouseEvent a
  | Resizing DragEvent a
  | AddDeck Cursor a
  | DeleteCell Cursor a
  | GrabStart DeckId DOM.MouseEvent a
  | Grabbing (DeckId × Cursor) DragEvent a
  | PreventDefault DOM.Event a
