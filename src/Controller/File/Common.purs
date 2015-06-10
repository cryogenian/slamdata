module Controller.File.Common where

import Data.Inject1 (Inject1, inj)
import Data.Maybe (Maybe(..))
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (Event())
import Input.File
import Model.File.Dialog

-- | Lifts an input value into an applicative and injects it into the right
-- | place in an Either.
toInput :: forall m a b. (Applicative m, Inject1 a b) => a -> m b
toInput = pure <<< inj

showError :: forall e. String -> Event (FileAppEff e) Input
showError = toInput <<< SetDialog <<< Just <<< ErrorDialog
