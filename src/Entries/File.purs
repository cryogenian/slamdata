module Entries.File where

import Utils
import Data.Tuple
import qualified Halogen as Hl
import qualified App as App
import qualified Controller.Driver as Cd
import Control.Monad.Aff
import Control.Monad.Aff.Queue
import Control.Monad.Eff.Class
import Utils
import Debug.Trace

main = onLoad $ void $ do
  Tuple node driver <- Hl.runUI App.app
  body <- bodyNode
  append body (convertToElement node)
  Cd.outside driver
