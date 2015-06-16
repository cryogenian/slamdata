module Controller.File.Common where

import Data.DOM.Simple.Encode (encodeURIComponent)
import Data.Inject1 (Inject1, inj)
import Data.Maybe (Maybe(), maybe)
import Data.Path.Pathy (printPath)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (Event())
import Input.File (Input(), FileInput(..))
import Model.File (_dialog)
import Model.File.Dialog (Dialog(..))
import Model.File.Salt (Salt(), runSalt)
import Model.File.Sort (Sort(), sort2string)
import Model.Path (DirPath())
import Optic.Core ((?~))

-- | Lifts an input value into an applicative and injects it into the right
-- | place in an Either.
toInput :: forall m a b. (Applicative m, Inject1 a b) => a -> m b
toInput = pure <<< inj

-- | Shows an error in a modal dialog.
-- | TODO: remove the need for this with unobtrusive errors #284
showError :: forall e. String -> Event (FileAppEff e) Input
showError msg = toInput $ WithState (_dialog ?~ ErrorDialog msg)

-- | Create a URL for the file browser using the specified values.
browseURL :: Maybe String -> Sort -> Salt -> DirPath -> String
browseURL search sort salt path =
  let search' = maybe "" (\s -> if s == "" then "" else s ++ " ") search
  in Config.browserUrl ++ "#?q=" ++ encodeURIComponent (search' ++ "path:\"" ++ printPath path ++ "\"")
                       ++ "&sort=" ++ sort2string sort
                       ++ "&salt=" ++ runSalt salt
