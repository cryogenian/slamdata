module Entries.Notebook where

import Ace.Types (ACE())
import App.Notebook (app)
import Control.Monad.Eff (Eff())
import Data.DOM.Simple.Navigator (platform)
import Data.DOM.Simple.Window (globalWindow, navigator)
import Data.Inject1 (prj)
import Data.Maybe (fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Platform (Platform(..))
import Data.String (indexOf)
import Data.Tuple (Tuple(..))
import EffectTypes (NotebookAppEff())
import Halogen (runUIWith)
import Input.Notebook (Input(..))
import Model.Notebook (_platform)
import Optic.Core ((.~))
import Utils (onLoad, mountUI)

import qualified App.Notebook.Ace as A
import qualified App.Notebook.ECharts as EC
import qualified Driver.Notebook as D
import qualified Driver.Notebook.Cell as DC

main :: Eff (NotebookAppEff (ace :: ACE)) Unit
main = onLoad $ void $ do
  aceKnot <- A.ref
  echartsKnot <- EC.ref
  Tuple node driver <- runUIWith app (postRender aceKnot echartsKnot)
  mountUI node
  platformName <- navigator globalWindow >>= platform
  let p = if indexOf "Win" platformName >= 0
          then Win
          else if indexOf "Mac" platformName >= 0
               then Mac
               else Other
  driver $ WithState (_platform .~ p)
  D.tickDriver driver
  D.driver aceKnot driver
  where
  postRender aKnot eKnot input node driver = do
    DC.driveCellContent input driver
    A.acePostRender aKnot input node driver
    EC.echartsPostRender eKnot input node driver
