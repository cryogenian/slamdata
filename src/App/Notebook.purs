module App.Notebook where

import Data.Void
import Control.Monad.Eff 
import qualified Halogen as H
import qualified Halogen.Signal as Hs

import qualified View.Notebook as V
import qualified Model.Notebook as M
import qualified Controller.Notebook as C
import qualified Input.Notebook as I

app :: forall e. H.UI M.Input Void M.Request e
app = {
  view: V.view <$> Hs.stateful M.initialState I.updateState,
  handler: C.handler,
  renderer: H.defaultRenderer
  }
       
