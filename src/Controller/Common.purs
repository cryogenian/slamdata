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

module Controller.Common where

import Prelude
import Api.Common (RetryEffects())
import Api.Fs (children)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Plus (empty)
import Data.Array (filter, mapMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Halogen.HTML.Events.Monad (Event(), andThen)
import Model.Resource
import Model.Path (DirPath())
import Network.HTTP.Affjax (AJAX())

getChildren :: forall i e. (Resource -> Boolean) -> (Array Resource -> Event (RetryEffects (ajax :: AJAX | e)) i) -> DirPath -> Event (RetryEffects (ajax :: AJAX | e)) i
getChildren pred f r = do
  ei <- liftAff $ attempt $ children r
  case ei of
    Right items -> do
      let items' = filter pred items
          parents = mapMaybe (either (const Nothing) Just <<< getPath) items
      f items' `andThen` \_ -> fold (getChildren pred f <$> parents)
    _ -> empty

getDirectories :: forall i e. (Array Resource -> Event (RetryEffects (ajax :: AJAX | e)) i) -> DirPath -> Event (RetryEffects (ajax :: AJAX | e)) i
getDirectories = getChildren (\x -> isDirectory x || isDatabase x)

getFiles :: forall i e. (Array Resource -> Event (RetryEffects (ajax :: AJAX | e)) i) -> DirPath -> Event (RetryEffects (ajax :: AJAX | e)) i
getFiles = getChildren isFile
