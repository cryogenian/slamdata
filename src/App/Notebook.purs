module App.Notebook where

import Halogen.Component (Component(), component)
import Halogen.Signal (stateful)
import Model.Notebook (Input(), initialState)
import Input.Notebook (updateState)
import View.Notebook (view)
import Halogen.HTML.Events.Monad (Event())
import EffectTypes (NotebookAppEff())

app :: forall p e. Component p (Event (NotebookAppEff e)) Input Input
app = component $ view <$> stateful initialState updateState
