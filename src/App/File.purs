module App.File (app) where

import Prelude
import EffectTypes (FileAppEff())
import Halogen.Component (Component())
import Halogen.HTML.Events.Monad (Event())
import Halogen.Signal (stateful)
import Input.File (Input(), updateState)
import Model.File (initialState)
import View.File (fileView)

app :: forall e. Component (Event (FileAppEff e)) Input Input
app = fileView <$> stateful initialState updateState
