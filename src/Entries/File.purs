module Entries.File where

import Api.Fs (mountInfo)
import App.File (app)
import Control.Monad.Aff (runAff, attempt)
import Control.Monad.Eff (Eff())
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Path.Pathy (rootDir)
import Data.Tuple (Tuple(..))
import Driver.File (outside)
import Driver.ZClipboard (initZClipboard)
import EffectTypes (FileAppEff())
import Halogen (runUIWith)
import Input.File
import Model.Resource (Resource(..))
import Utils (onLoad, mountUI)

main :: Eff (FileAppEff ()) Unit
main = onLoad $ void $ do
  Tuple node driver <- runUIWith app postRender
  mountUI node
  outside driver
  flip (runAff (const $ pure unit)) (attempt $ mountInfo (Database rootDir)) \result -> do
    case result of
      Left _ -> pure unit
      Right _ -> driver $ inj (SetHasMountRoot true)
  where
  postRender _ node _ = initZClipboard node
