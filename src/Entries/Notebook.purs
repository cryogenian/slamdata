module Entries.Notebook where

import Ace.Types (EAce())
import App.Notebook (app)
import App.Notebook.Ace (acePostRender, ref)
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

main :: Eff (NotebookAppEff (ace :: EAce)) Unit
main = onLoad $ void $ do
  m <- ref
  Tuple node driver <- runUIWith app (postRender m)
  mountUI node
  D.tickDriver driver
  D.driver m driver
  where
  postRender m input = fromMaybe (\_ _ -> return unit) $ acePostRender m <$> prj input
