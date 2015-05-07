module Entries.Notebook where

import App.Notebook (app)
import App.Notebook.Ace (acePostRender)
import Control.Monad.Eff (Eff())
import Data.Tuple (Tuple(..))
import EffectTypes (NotebookAppEff())
import Halogen (runUIWith)
import Utils (onLoad, mountUI)
import Ace.Types (EAce())
import Control.Monad.Eff.Ref (newRef)
import qualified Data.StrMap as M
import qualified Driver.Notebook as D

main :: Eff (NotebookAppEff (ace :: EAce)) Unit
main = onLoad $ void $ do
  m <- newRef M.empty
  Tuple node driver <- runUIWith app (acePostRender m)
  mountUI node
  D.driver driver
