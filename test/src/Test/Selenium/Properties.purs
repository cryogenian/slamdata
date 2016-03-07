module Test.Selenium.Properties where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Test.Selenium.Feature (Property())

value :: Maybe String -> Property
value = Tuple "value"

untitledNotebookValue :: Property
untitledNotebookValue = value (Just "Untitled Notebook")
