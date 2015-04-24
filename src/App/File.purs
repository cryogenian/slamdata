module App.File (app) where

import EffectTypes (FileAppEff())
import Halogen.Component (Component(), component)
import Halogen.HTML.Events.Monad (Event())
import Halogen.Signal (stateful)
import Input.File (Input(), updateState)
import Model.File (initialState)
import View.File (view)

app :: forall p e. Component p (Event (FileAppEff e)) Input Input
app = component (view <$> stateful initialState updateState)
