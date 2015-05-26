module Entries.Notebook where

import Ace.Types (EAce())
import App.Notebook (app)
import Control.Monad.Eff (Eff())
import Data.Date (Now())
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

main :: Eff (NotebookAppEff (ace :: EAce)) Unit
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
    A.acePostRender aKnot input node driver
    EC.echartsPostRender eKnot input node driver
