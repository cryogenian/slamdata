module App.File (app) where

import Control.Alternative (Alternative)
import Control.Monad.Aff (Aff())
import Controller.File (handler)
import EffectTypes (FileAppEff())
import Halogen.Component (Component(), component)
import Halogen.HTML.Events.Monad (Event(), async)
import Halogen.Signal (stateful)
import Input.File (inner)
import Model.File (Input(), Request(), initialState)
import View.File (view)

app :: forall p e. Component p (Event (FileAppEff e)) Input Input
app = component (view handler <$> stateful initialState inner)
