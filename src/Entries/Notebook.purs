{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Entries.Notebook where

import Prelude
import Ace.Types (ACE())
import App.Notebook (app)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (modifyRef, newRef)
import Control.Timer (timeout)
import Data.DOM.Simple.Navigator (platform)
import Data.DOM.Simple.Window (globalWindow, navigator)
import Data.Inject1 (prj)
import Data.Maybe (fromMaybe, Maybe(..), isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Platform (Platform(..))
import Data.String (indexOf)
import Data.Tuple (Tuple(..), snd)
import Driver.ZClipboard (initZClipboard)
import EffectTypes (NotebookAppEff())
import Entries.Common (setSlamDataTitle, getVersion)
import Halogen (runUIWith)
import Input.Notebook (Input(..), updateState)
import Model.Notebook (_platform, _version, initialState)
import Optic.Core
import Utils (onLoad, mountUI)

import qualified Driver.Notebook.Ace as A
import qualified Driver.Notebook.ECharts as EC
import qualified Data.Map as M
import qualified Driver.Notebook as D
import qualified Driver.Notebook.Cell as DC
import qualified Driver.Notebook.Notify as N
import qualified DOM.BrowserFeatures.Detectors as BFD

main :: Eff _ Unit
main = onLoad $ void $ do
  browserFeatures <- BFD.detectBrowserFeatures
  stateKnot <- newRef (initialState browserFeatures)
  aceKnot <- newRef M.empty
  t <- timeout 0 (pure unit)
  autosaveTimer <- newRef t
  echartsKnot <- EC.ref
  notifyKnot <- newRef M.empty
  let post = postRender stateKnot aceKnot echartsKnot autosaveTimer notifyKnot
  Tuple node driver <- runUIWith (app browserFeatures) post
  mountUI node
  platformName <- navigator globalWindow >>= platform
  let p = if isJust $ indexOf "Win" platformName
          then Win
          else if isJust $ indexOf "Mac" platformName
               then Mac
               else Other
  driver $ WithState (_platform .~ p)
  D.tickDriver driver
  D.driver stateKnot driver
  D.handleShortcuts stateKnot driver
  launchAff do
    version <- getVersion
    liftEff $ setSlamDataTitle version
    liftEff $ driver $ WithState (_version .~ version)
  where
  postRender sKnot aKnot eKnot autosaveTimer notifyKnot input node driver = do
    modifyRef sKnot (flip updateState input)
    initZClipboard node
    D.notebookAutosave sKnot autosaveTimer input driver
    DC.driveCellContent input driver
    A.acePostRender sKnot aKnot input node driver
    EC.echartsPostRender eKnot input node driver
    N.notifyDriver sKnot notifyKnot input driver

