module Entries.Notebook where

import App.Notebook (app)
import App.Notebook.Ace (acePostRender, ref)
import Control.Monad.Eff (Eff())
import Data.Date (Now())
import Data.Tuple (Tuple(..))
import EffectTypes (NotebookAppEff())
import Halogen (runUIWith)
import Utils (onLoad, mountUI)
import Ace.Types (EAce())
import qualified Driver.Notebook as D



main :: Eff (NotebookAppEff (ace :: EAce)) Unit
main = onLoad $ void $ do
  m <- ref
  Tuple node driver <- runUIWith app (acePostRender m)
  mountUI node
  D.tickDriver driver
  D.driver m driver
