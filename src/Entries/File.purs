module Entries.File where

import Prelude
import App.File (app)
import Optic.Core
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Data.Tuple (Tuple(..))
import Input.File (FileInput(..))
import Model.File (_version)
import Data.Inject1 (inj)
import Driver.File (outside)
import Driver.ZClipboard (initZClipboard)
import EffectTypes (FileAppEff())
import Entries.Common (setSlamDataTitle, getVersion)
import Halogen (runUIWith)
import Utils (onLoad, mountUI, setDocumentTitle)

main :: Eff _ Unit
main = onLoad $ void $ do
  Tuple node driver <- runUIWith app postRender
  mountUI node
  outside driver
  launchAff do
    version <- getVersion
    liftEff $ setSlamDataTitle version
    liftEff $ driver $ inj $ WithState (_version .~ version)
  where
  postRender _ node _ = initZClipboard node 
