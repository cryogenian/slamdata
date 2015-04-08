module App.File where

import Data.Void
import qualified Utils.File as Uf
import qualified Model.File as M
import qualified View.File as V
import qualified Input.File as Ci
import qualified Controller.File as Cr
import qualified Control.Timer as Tm
import qualified Halogen as Hl
import qualified Halogen.Signal as HS
import qualified Network.HTTP.Affjax as Af


app :: forall e. Hl.UI M.Input Void M.Request
       (timer::Tm.Timer, file::Uf.ReadFile, ajax::Af.Ajax|e)
app = {
  view: V.view <$> HS.stateful M.initialState Ci.inner,
  handler: Cr.handler,
  renderer: Hl.defaultRenderer
  }
