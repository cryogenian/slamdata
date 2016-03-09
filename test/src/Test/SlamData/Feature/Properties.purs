module Test.SlamData.Feature.Properties where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Test.Feature (Property())

value :: Maybe String -> Property
value = Tuple "value"

untitledNotebookValue :: Property
untitledNotebookValue = value (Just "Untitled Notebook")
