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

module SlamData.Notebook.Menu.Component.Query where

import SlamData.Prelude

import Halogen.Menu.Component as HalogenMenu

import SlamData.Notebook.Dialog.Component as Dialog
import SlamData.Notebook.Editor.Component.Query as Notebook
import SlamData.Notebook.Rename.Component as Rename

type FromMenuQuery =
  Coproduct Rename.Query (Coproduct Dialog.Query Notebook.Query)

newtype HelpURI = HelpURI String

type Value = Either HelpURI (FromMenuQuery Unit)

type QueryP = HalogenMenu.MenuQueryP (Maybe Value)

helpURIToValue :: HelpURI -> Value
helpURIToValue = Left

dialogQueryToValue :: Dialog.Query Unit -> Value
dialogQueryToValue = left >>> right >>> Right

notebookQueryToValue :: Notebook.Query Unit -> Value
notebookQueryToValue = right >>> right >>> Right

renameQueryToValue :: Rename.Query Unit -> Value
renameQueryToValue = left >>> Right
