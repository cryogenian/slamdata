module Entries.Notebook where

import Prelude
import Ace.Types (ACE())
import App.Notebook (app)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff())
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
import Entries.Common (setSlamDataTitle)
import Halogen (runUIWith)
import Input.Notebook (Input(..), updateState)
import Model.Notebook (_platform, initialState)
import Optic.Setter ((.~))
import Utils (onLoad, mountUI)

import qualified Driver.Notebook.Ace as A
import qualified Driver.Notebook.ECharts as EC
import qualified Data.Map as M
import qualified Driver.Notebook as D
import qualified Driver.Notebook.Cell as DC
import qualified Driver.Notebook.Notify as N

main :: Eff (NotebookAppEff (ace :: ACE)) Unit
main = onLoad $ void $ do
  launchAff setSlamDataTitle
  stateKnot <- newRef initialState
  aceKnot <- newRef M.empty
  t <- timeout 0 (pure unit)
  autosaveTimer <- newRef t
  echartsKnot <- EC.ref
  notifyKnot <- newRef M.empty
  let post = postRender stateKnot aceKnot echartsKnot autosaveTimer notifyKnot
  Tuple node driver <- runUIWith app post
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
  where
  postRender sKnot aKnot eKnot autosaveTimer notifyKnot input node driver = do
    modifyRef sKnot (flip updateState input)
    initZClipboard node
    D.notebookAutosave sKnot autosaveTimer input driver
    DC.driveCellContent input driver
    A.acePostRender sKnot aKnot input node driver
    EC.echartsPostRender eKnot input node driver
    N.notifyDriver sKnot notifyKnot input driver

