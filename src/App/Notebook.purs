module App.Notebook where

import App.Notebook.Ace (ui)
import Halogen.Component (Component(), combine)
import Halogen.Signal (stateful)
import Halogen.HTML (HTML())
import Model.Notebook (Input(RunCell), initialState)
import Input.Notebook (updateState)
import View.Notebook (view)
import Halogen.HTML.Events.Monad (Event())
import EffectTypes (NotebookAppEff())

import Data.Profunctor (dimap)
import Data.Either (Either(..), either)

app :: forall e. Component (Event (NotebookAppEff e)) Input Input
app = dimap Left (((either id id) <$>) <$>) $ combine todo (view <$> stateful initialState updateState) (ui "TODO")
  where todo :: forall a. HTML a -> HTML a -> HTML a
        todo = const
