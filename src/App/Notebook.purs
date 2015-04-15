module App.Notebook where

import Halogen.Component (Component(), component)
import Halogen.Signal (stateful)
import Input.Notebook (updateState)
import Model.Notebook (Input(), initialState)
import View.Notebook (view)

app :: forall p m. (Applicative m) => Component p m Input Input
app = component $ view <$> stateful initialState updateState
