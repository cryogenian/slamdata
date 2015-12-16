module Dashboard.Menu.Component.Query where

import Prelude

import Data.Functor.Coproduct (Coproduct(), left, right)
import Data.Either (Either(..))
import Data.Maybe (Maybe())

import Halogen.Menu.Component as HalogenMenu

import Dashboard.Dialog.Component as Dialog
import Notebook.Component as Notebook

type DialogOrNotebookQuery = Coproduct Dialog.Query Notebook.NotebookQuery

newtype HelpURI = HelpURI String

type Value = Either HelpURI (DialogOrNotebookQuery Unit)

type QueryP = HalogenMenu.MenuQueryP (Maybe Value)

helpURIToValue :: HelpURI -> Value
helpURIToValue = Left

dialogQueryToValue :: Dialog.Query Unit -> Value
dialogQueryToValue = Right <<< left

notebookQueryToValue :: Notebook.NotebookQuery Unit -> Value
notebookQueryToValue = Right <<< right

