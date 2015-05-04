module Entries.Notebook where

import App.Notebook (app)
import Control.Monad.Eff (Eff())
import Data.Tuple (Tuple(..))
import EffectTypes (NotebookAppEff())
import Halogen (runUI)
import Utils (onLoad, mountUI)
import Ace.Types (EAce())
import qualified Driver.Notebook as D

main :: Eff (NotebookAppEff (ace :: EAce)) Unit
main = onLoad $ void $ do
  Tuple node driver <- runUI app
  mountUI node
  D.driver driver
