module Entries.File where

import Utils
import Data.Tuple
import qualified Halogen as Hl
import qualified App.File as App
import qualified Driver.File as D
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Utils
import Debug.Trace

main = onLoad $ void $ do
  Tuple node driver <- Hl.runUI App.app
  body <- bodyNode
  append body (convertToElement node)
  D.outside driver
