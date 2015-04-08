module Entries.Notebook where

import Debug.Trace
import Utils
import Data.Tuple
import qualified Halogen as Hl
import qualified App.Notebook as App
import qualified Driver.Notebook as D

main = onLoad $ void $ do
  Tuple node driver <- Hl.runUI App.app
  body <- bodyNode
  append body (convertToElement node)
  D.driver driver
       
