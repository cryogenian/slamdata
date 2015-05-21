module Entries.Notebook where

import Ace.Types (EAce())
import App.Notebook (app)
import Control.Monad.Eff (Eff())
import Data.Date (Now())
import Data.Inject1 (prj)
import Data.Maybe (fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..))
import EffectTypes (NotebookAppEff())
import Halogen (runUIWith)
import Utils (onLoad, mountUI)
import qualified Driver.Notebook as D
import qualified App.Notebook.Ace as A
import qualified App.Notebook.ECharts as EC


main :: Eff (NotebookAppEff (ace :: EAce)) Unit
main = onLoad $ void $ do
  aceKnot <- A.ref
  echartsKnot <- EC.ref 
  Tuple node driver <- runUIWith app (postRender aceKnot echartsKnot)
  mountUI node
  D.tickDriver driver
  D.driver aceKnot driver
  where
  postRender aKnot eKnot input node driver = do
    A.acePostRender aKnot input node driver
    EC.echartsPostRender eKnot input node driver 
