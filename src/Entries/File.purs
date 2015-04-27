module Entries.File where

import App.File (app)
import Control.Monad.Eff (Eff())
import Data.Tuple (Tuple(..))
import Driver.File (outside)
import EffectTypes (FileAppEff())
import Halogen (runUI)
import Utils (onLoad, mountUI)

main :: Eff (FileAppEff ()) Unit
main = onLoad $ void $ do
  Tuple node driver <- runUI app
  mountUI node
  outside driver
