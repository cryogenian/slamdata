module App.Notebook where

import Halogen.Component (Component())
import Halogen.Signal (stateful)
import Model.Notebook (Input(), initialState)
import Input.Notebook (updateState)
import View.Notebook (view)
import Halogen.HTML.Events.Monad (Event())
import EffectTypes (NotebookAppEff())

app :: forall e. Component (Event (NotebookAppEff e)) Input Input
app = view <$> stateful initialState updateState
