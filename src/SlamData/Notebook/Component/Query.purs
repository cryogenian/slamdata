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

module SlamData.Notebook.Component.Query where

import Prelude (Unit(), unit, (<<<), ($))

import Data.Functor.Coproduct (Coproduct(), left, right)
import Data.Maybe (Maybe())

import DOM.Event.EventTarget (EventListener())

import Halogen
import Halogen.Component.ChildPath

import SlamData.Notebook.AccessType (AccessType())
import SlamData.Notebook.Cell.CellId (CellId())
import SlamData.Notebook.Component.ChildSlot
import SlamData.Notebook.Editor.Component.Query as Notebook
import SlamData.Notebook.Effects (NotebookEffects())
import SlamData.Notebook.Menu.Component.Query as Menu
import SlamData.Notebook.Rename.Component as Rename

data Query a
  = ActivateKeyboardShortcuts a
  | DeactivateKeyboardShortcuts a
  | EvaluateMenuValue Menu.Value a
  | AddKeyboardListener (EventListener NotebookEffects) a
  | SetAccessType AccessType a
  | GetAccessType (AccessType -> a)
  | SetViewingCell (Maybe CellId) a
  | GetViewingCell (Maybe CellId -> a)
  | SetParentHref String a
  | DismissAll a

type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

toDraftboard :: (Unit -> Query Unit) -> QueryP Unit
toDraftboard = left <<< action

fromDraftboard
  :: forall a. (forall i. (a -> i) -> Query i) -> QueryP a
fromDraftboard r = left $ request r

toNotebook :: (Unit -> Notebook.Query Unit) -> QueryP Unit
toNotebook =
      right
  <<< ChildF (injSlot cpNotebook unit)
  <<< right
  <<< right
  <<< right
  <<< left
  <<< action

fromNotebook
  :: forall a. (forall i. (a -> i) -> Notebook.Query i) -> QueryP a
fromNotebook r =
    right
  $ ChildF (injSlot cpNotebook unit)
  $ right
  $ right
  $ right
  $ left
  $ request r

toRename :: (Unit -> Rename.Query Unit) -> QueryP Unit
toRename =
      right
  <<< ChildF (injSlot cpRename unit)
  <<< left
  <<< action

fromRename
  :: forall a. (forall i. (a -> i) -> Rename.Query i) -> QueryP a
fromRename r =
    right
  $ ChildF (injSlot cpRename unit)
  $ left
  $ request r
