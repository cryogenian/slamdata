module Entries.File where

import App.File (app)
import Control.Monad.Eff (Eff())
import Control.Timer (Timer())
import Data.Tuple (Tuple(..))
import Driver.File (outside)
import EffectTypes (FileAppEff())
import Halogen (runUI)
import Network.HTTP.Affjax (AJAX())
import Utils (onLoad, append, bodyNode, convertToElement)
import Utils.File (ReadFile())

main :: Eff (FileAppEff ()) Unit
main = onLoad $ void $ do
  Tuple node driver <- runUI app
  bodyNode >>= flip append (convertToElement node) 
  outside driver

