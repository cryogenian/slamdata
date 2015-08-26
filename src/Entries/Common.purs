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

module Entries.Common where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Data.Foreign.Class (readProp)
import Data.Path.Pathy (rootDir, dir, file, (</>))
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import DOM (DOM())
import Network.HTTP.Affjax (AJAX(), get)
import Utils (setDocumentTitle)

import Api.Common (RetryEffects(..), retryGet)

import qualified Data.DOM.Simple.Window as W

setSlamDataTitle :: forall eff. Maybe String -> Eff (dom :: DOM | eff) Unit
setSlamDataTitle maybeVersion = do
  let version = maybe "" (" " ++) maybeVersion
  setDocumentTitle $ "SlamData" <> version

getVersion :: forall eff. Aff (RetryEffects (ajax :: AJAX | eff)) (Maybe String)
getVersion = do
  serverInfo <- retryGet Config.Paths.serverInfoUrl  
  return $ either (const Nothing) Just (readProp "version" serverInfo.response)
