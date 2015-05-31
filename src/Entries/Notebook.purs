module Entries.Notebook where

import Ace.Types (ACE())
import App.Notebook (app)
import Control.Timer (timeout)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (modifyRef, newRef)
import Data.DOM.Simple.Navigator (platform)
import Data.DOM.Simple.Window (globalWindow, navigator)
import Data.Inject1 (prj)
import Data.Maybe (fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Platform (Platform(..))
import Data.String (indexOf)
import Data.Tuple (Tuple(..), snd)
import EffectTypes (NotebookAppEff())
import Halogen (runUIWith)
import Input.Notebook (Input(..), updateState)
import Model.Notebook (_platform, initialState)
import Optic.Core ((.~))
import Utils (onLoad, mountUI)

import qualified App.Notebook.Ace as A
import qualified App.Notebook.ECharts as EC
import qualified Data.Map as M
import qualified Driver.Notebook as D
import qualified Driver.Notebook.Cell as DC

main :: Eff (NotebookAppEff (ace :: ACE)) Unit
main = onLoad $ void $ do
  stateKnot <- newRef initialState
  aceKnot <- newRef M.empty
  t <- timeout 0 (pure unit)
  autosaveTimer <- newRef t
  echartsKnot <- EC.ref
  let post = postRender stateKnot aceKnot echartsKnot autosaveTimer
  Tuple node driver <- runUIWith app post
  mountUI node
  platformName <- navigator globalWindow >>= platform
  let p = if indexOf "Win" platformName >= 0
          then Win
          else if indexOf "Mac" platformName >= 0
               then Mac
               else Other
  driver $ WithState (_platform .~ p)
  D.tickDriver driver
  D.driver stateKnot driver
  where
  postRender sKnot aKnot eKnot autosaveTimer input node driver = do
    modifyRef sKnot (flip updateState input)
    D.notebookAutosave sKnot autosaveTimer input driver
    DC.driveCellContent input driver
    A.acePostRender aKnot input node driver
    EC.echartsPostRender eKnot input node driver
