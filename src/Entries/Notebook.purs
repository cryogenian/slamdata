module Entries.Notebook where

import App.Notebook (app)
import Data.Tuple (Tuple(..))
import Halogen (runUI)
import Utils (onLoad, mountUI)
import qualified Driver.Notebook as D

main = onLoad $ void $ do
  Tuple node driver <- runUI app
  mountUI node
  D.driver driver
