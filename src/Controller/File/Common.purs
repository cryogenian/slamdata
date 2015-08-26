{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Controller.File.Common where

import Prelude
import Utils (encodeURIComponent)
import Data.Inject1 (Inject1, inj)
import Data.Maybe (Maybe(), maybe)
import Data.Path.Pathy (printPath)
import EffectTypes (FileAppEff())
import Input.File (Input(), FileInput(..))
import Model.File (_dialog)
import Model.File.Dialog (Dialog(..))
import Model.File.Salt (Salt(), runSalt)
import Model.File.Sort (Sort(), sort2string)
import Model.Path (DirPath())
import Optic.Core

import qualified Halogen.HTML.Events.Monad as E

type Event e = E.Event (FileAppEff e) Input

-- | Lifts an input value into an applicative and injects it into the right
-- | place in an Either.
toInput :: forall m a b. (Applicative m, Inject1 a b) => a -> m b
toInput = pure <<< inj

-- | Shows an error in a modal dialog.
-- | TODO: remove the need for this with unobtrusive errors #284
showError :: forall e. String -> Event e
showError msg = toInput $ WithState (_dialog ?~ ErrorDialog msg)

-- | Create a URL for the file browser using the specified values.
browseURL :: Maybe String -> Sort -> Salt -> DirPath -> String
browseURL search sort salt path =
  let search' = maybe "" (\s -> if s == "" then "" else s ++ " ") search
  in Config.browserUrl ++ "#?q=" ++ encodeURIComponent (search' ++ "path:\"" ++ printPath path ++ "\"")
                       ++ "&sort=" ++ sort2string sort
                       ++ "&salt=" ++ runSalt salt
