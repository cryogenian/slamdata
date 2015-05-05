module App.File (app) where

import EffectTypes (FileAppEff())
import Halogen.Component (Component())
import Halogen.HTML.Events.Monad (Event())
import Halogen.Signal (stateful)
import Input.File (Input(), updateState)
import Model.File (initialState)
import View.File (view)

app :: forall e. Component (Event (FileAppEff e)) Input Input
app = view <$> stateful initialState updateState
