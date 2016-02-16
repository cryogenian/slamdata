{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook (main) where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff(), runAff, forkAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Either (Either(..))
import Data.Functor.Eff (liftEff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Ace.Config as AceConfig

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen (Driver(), runUI, installedState)
import Halogen.Util (appendToBody, onLoad)

import SlamData.Config as Config
import SlamData.FileSystem.Routing (parentURL)
import SlamData.Notebook.Action (Action(..), toAccessType)
import SlamData.Notebook.Cell.CellId as CID
import SlamData.Notebook.Cell.Port as Port
import SlamData.Notebook.Component as Draftboard
import SlamData.Notebook.Editor.Component as Notebook
import SlamData.Effects (SlamDataRawEffects(), SlamDataEffects())
import SlamData.Notebook.Rename.Component as Rename
import SlamData.Notebook.Routing (Routes(..), routing)

import Utils.Path as UP

main :: Eff SlamDataEffects Unit
main = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ++ "js/ace")
  AceConfig.set AceConfig.modePath (Config.baseUrl ++ "js/ace")
  AceConfig.set AceConfig.themePath (Config.baseUrl ++ "js/ace")
  browserFeatures <- detectBrowserFeatures
  runAff throwException (const (pure unit)) $ do
    app <- runUI Draftboard.comp
      $ installedState
      $ Draftboard.initialState { browserFeatures: browserFeatures }
    onLoad (appendToBody app.node)
    forkAff (routeSignal app.driver)

routeSignal
  :: Driver Draftboard.QueryP SlamDataRawEffects
  -> Aff SlamDataEffects Unit
routeSignal driver = do
  Tuple _ route <- Routing.matchesAff' UP.decodeURIPath routing
  case route of
    CellRoute res cellId accessType varMap ->
      notebook res (Load accessType) (Just cellId) varMap
    NotebookRoute res action varMap -> notebook res action Nothing varMap
    ExploreRoute res -> explore res

  where

  explore :: UP.FilePath -> Aff SlamDataEffects Unit
  explore path = do
    fs <- liftEff detectBrowserFeatures
    driver $ Draftboard.toNotebook $ Notebook.ExploreFile fs path
    driver $ Draftboard.toDraftboard $ Draftboard.SetParentHref
      $ parentURL $ Left path
    driver $ Draftboard.toRename $ Rename.SetText $ Config.newNotebookName

  notebook
    :: UP.DirPath
    -> Action
    -> Maybe CID.CellId
    -> Port.VarMap
    -> Aff SlamDataEffects Unit
  notebook path action viewing varMap = do
    let name = UP.getNameStr $ Right path
        accessType = toAccessType action
    currentPath <- driver $ Draftboard.fromNotebook Notebook.GetNotebookPath
    currentVarMap <- driver $ Draftboard.fromNotebook Notebook.GetGlobalVarMap
    currentViewing <- driver $ Draftboard.fromDraftboard Draftboard.GetViewingCell
    currentAccessType <- driver $ Draftboard.fromDraftboard Draftboard.GetAccessType

    when (currentPath /= pure path) do
      features <- liftEff detectBrowserFeatures
      if action == New
        then driver $ Draftboard.toNotebook $ Notebook.Reset features path
        else driver $ Draftboard.toNotebook $ Notebook.LoadNotebook features path

    driver $ Draftboard.toRename $ Rename.SetText $ UP.dropNotebookExt name
    driver $ Draftboard.toDraftboard $ Draftboard.SetViewingCell viewing
    driver $ Draftboard.toDraftboard $ Draftboard.SetAccessType accessType
    driver $ Draftboard.toNotebook $ Notebook.SetGlobalVarMap varMap
    driver $ Draftboard.toDraftboard $ Draftboard.SetParentHref
      $ parentURL $ Right path
