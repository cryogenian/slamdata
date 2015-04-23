module Entries.Notebook where

import App.Notebook (app)
import Data.Tuple (Tuple(..))
import Halogen (runUI)
import Utils (onLoad, append, bodyNode, convertToElement)
import qualified Driver.Notebook as D

main = onLoad $ void $ do
  Tuple node driver <- runUI app
  bodyNode >>= flip append (convertToElement node)
  D.driver driver
