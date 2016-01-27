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

module Dashboard.Component.Query where

import Data.Maybe (Maybe())

import DOM.Event.EventTarget (EventListener())

import Dashboard.Menu.Component.Query as Menu
import Model.AccessType (AccessType())
import Notebook.Cell.CellId (CellId())
import Notebook.Effects (NotebookEffects())

data Query a
  = ActivateKeyboardShortcuts a
  | DeactivateKeyboardShortcuts a
  | EvaluateMenuValue Menu.Value a
  | AddKeyboardListener (EventListener NotebookEffects) a
  | Save a
  | SetAccessType AccessType a
  | GetAccessType (AccessType -> a)
  | SetViewingCell (Maybe CellId) a
  | GetViewingCell (Maybe CellId -> a)
  | SetParentHref String a
  | DismissAll a
