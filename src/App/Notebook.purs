module App.Notebook where

import Prelude
import Halogen.Component (Component())
import Halogen.Signal (stateful)
import Model.Notebook (initialState)
import Input.Notebook (Input(), updateState)
import View.Notebook (notebookView)
import Halogen.HTML.Events.Monad (Event())
import EffectTypes (NotebookAppEff())

app :: forall e. Component (Event (NotebookAppEff e)) Input Input
app = notebookView <$> stateful initialState updateState
