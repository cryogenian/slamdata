module View.Notebook.Common (HTML()) where

import Input.Notebook (Input())
import EffectTypes (NotebookAppEff())
import Halogen.HTML.Events.Monad (Event())
import qualified Halogen.HTML as H

type HTML e = H.HTML (Event (NotebookAppEff e) Input)
