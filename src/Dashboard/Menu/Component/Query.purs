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

module Dashboard.Menu.Component.Query where

import Prelude

import Data.Functor.Coproduct (Coproduct(), left, right)
import Data.Either (Either(..))
import Data.Maybe (Maybe())

import Halogen.Menu.Component as HalogenMenu

import Dashboard.Dialog.Component as Dialog
import Dashboard.Rename.Component as Rename
import Notebook.Component as Notebook

type FromMenuQuery =
  Coproduct Rename.Query (Coproduct Dialog.Query Notebook.NotebookQuery)

newtype HelpURI = HelpURI String

type Value = Either HelpURI (FromMenuQuery Unit)

type QueryP = HalogenMenu.MenuQueryP (Maybe Value)

helpURIToValue :: HelpURI -> Value
helpURIToValue = Left

dialogQueryToValue :: Dialog.Query Unit -> Value
dialogQueryToValue = left >>> right >>> Right

notebookQueryToValue :: Notebook.NotebookQuery Unit -> Value
notebookQueryToValue = right >>> right >>> Right

renameQueryToValue :: Rename.Query Unit -> Value
renameQueryToValue = left >>> Right
