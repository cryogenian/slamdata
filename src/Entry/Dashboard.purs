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

module Entry.Dashboard where

import Prelude

import Control.Monad.Aff (runAff, forkAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen (runUI, installedState)
import Halogen.Util (appendToBody, onLoad)

import Ace.Config as AceConfig

import Dashboard.Component (comp, initialState)
import Dashboard.Routing (routeSignal)
import Dashboard.Autosave (autoSaveSignal)
import Notebook.Effects (NotebookEffects())

main :: Eff NotebookEffects Unit
main = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ++ "js/ace")
  browserFeatures <- detectBrowserFeatures
  runAff throwException (const (pure unit)) $ do
    app <- runUI comp $ installedState $ initialState browserFeatures
    onLoad (appendToBody app.node)
    forkAff $ routeSignal app.driver
    forkAff $ autoSaveSignal app.driver
