module App.Notebook where

import Halogen.Component (Component(), component, hide, install, mapP)
import Halogen.Signal (stateful)
import Model.Notebook (Input(AceInput), initialState)
import Input.Notebook (updateState)
import View.Notebook (Placeholder(..), view)
import Halogen.HTML.Events.Monad (Event())
import EffectTypes (NotebookAppEff())

import Ace.Types (EAce())
import Data.Either (Either(), either)
import Data.Maybe (Maybe(Nothing))
import Halogen (HalogenEffects())
import Halogen.Internal.VirtualDOM (Widget())
import qualified App.Notebook.Ace as NA

-- TODO: Investigate why I can't extract intermediate functions from this.
app :: forall p e. Component (Widget (HalogenEffects (ace :: EAce | p)) Input) (Event (NotebookAppEff e)) Input Input
app = mapP (AceInput <$>) <<< hide <<< install NA.ui <<< mapP replacePlaceholder <<< component $ view <$> stateful initialState updateState

replacePlaceholder :: forall p. Placeholder -> Maybe p
replacePlaceholder AceEditor = Nothing
