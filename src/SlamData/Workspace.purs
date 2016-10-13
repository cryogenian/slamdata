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

module SlamData.Workspace (main) where

import SlamData.Prelude

import Control.Coroutine (runProcess, await, ($$))
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.UI.Browser as Browser

import Ace.Config as AceConfig

import Data.Nullable (toMaybe)

import DOM.HTML.Document (body)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.Element (setClassName)
import DOM.HTML.Types (htmlElementToElement)

import Halogen (Driver, runUI, parentState, interpret)
import Halogen.Util (runHalogenAff, awaitBody)

import SlamData.Analytics as Analytics
import SlamData.Config as Config
import SlamData.Effects (SlamDataRawEffects, SlamDataEffects)
import SlamData.Monad (runSlam)
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action (Action(..), toAccessType)
import SlamData.Workspace.Component as Workspace
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Routing (Routes(..), routing)
import SlamData.Workspace.StyleLoader as StyleLoader
import SlamData.Wiring (makeWiring)

import Routing as Routing

import Utils.Path as UP

import Data.Map as Map

main ∷ Eff SlamDataEffects Unit
main = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.modePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.themePath (Config.baseUrl ⊕ "js/ace")
  runHalogenAff do
    forkAff Analytics.enableAnalytics
    let st = Workspace.initialState (Just "3.0")
    wiring ← makeWiring
    let ui = interpret (runSlam wiring) Workspace.comp
    driver ← runUI ui (parentState st) =<< awaitBody
    forkAff (routeSignal driver)
  StyleLoader.loadStyles

routeSignal
  ∷ Driver Workspace.QueryP SlamDataRawEffects
  → Aff SlamDataEffects Unit
routeSignal driver =
  runProcess (routeProducer $$ routeConsumer Nothing)

  where
  routeProducer = produce \emit →
    Routing.matches' UP.decodeURIPath routing \_ → emit ∘ Left

  routeConsumer old = do
    new ← await
    traceAnyA new
    case new of
      WorkspaceRoute path deckId action varMaps → lift do
        traceAnyA "OOOH"
        traverse_ traceAnyA $ Map.toList varMaps
        traceAnyA "OOOH\n"
        driver $ Workspace.toWorkspace $ Workspace.SetVarMaps varMaps
        traceAnyA "route"
        traverse_ traceAnyA $ Map.toList varMaps
        traceAnyA "route\n"

        case old of
          Just (WorkspaceRoute path' deckId' action' varMaps')
            | path == path' && deckId == deckId' && action == action' && varMaps == varMaps' →
                pure unit
          _ → do
            when (toAccessType action == AT.ReadOnly) do
              isEmbedded ← liftEff $ Browser.detectEmbedding
              let bodyClass = if isEmbedded then "sd-workspace-page sd-embedded" else "sd-workspace-page"
              void $ liftEff $
                traverse (setClassName bodyClass <<< htmlElementToElement)
                  <<< toMaybe
                  =<< body
                  =<< document
                  =<< window
            workspace path deckId action

    routeConsumer (Just new)

  workspace
    ∷ UP.DirPath
    → Maybe DeckId
    → Action
    → Aff SlamDataEffects Unit
  workspace path deckId =
    case _ of
      New →
        driver $ Workspace.toWorkspace $ Workspace.Reset path
      Load accessType →
        driver $ Workspace.toWorkspace $ Workspace.Load path deckId accessType
      Exploring fp → do
        driver $ Workspace.toWorkspace $ Workspace.Reset path
        driver $ Workspace.toDeck $ Deck.ExploreFile fp
