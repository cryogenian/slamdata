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
import Control.Monad.Aff.Free (fromEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (writeRef)
import Control.UI.Browser as Browser

import Ace.Config as AceConfig

import Data.Map.Diff as Diff
import Data.Nullable (toMaybe)

import DOM.HTML.Document (body)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlElementToElement, htmlElementToNode, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Element (setClassName)
import DOM.Node.Node (removeChild)
import DOM.Node.Types (elementToNode)
import DOM.Node.ParentNode (querySelector)

import Halogen (Driver, runUI, parentState, interpret)
import Halogen.Util (runHalogenAff, awaitBody)

import SlamData.Config as Config
import SlamData.Effects (SlamDataRawEffects, SlamDataEffects)
import SlamData.Monad (runSlam)
import SlamData.Wiring (Wiring(..))
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action (Action(..), toAccessType)
import SlamData.Workspace.Component as Workspace
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Persistence as P
import SlamData.Workspace.Eval.Traverse (resolveUrlVarMaps)
import SlamData.Workspace.Routing (Routes(..), routing)
import SlamData.Workspace.StyleLoader as StyleLoader
import SlamData.Quasar.Auth.Permission as Permission

import Routing as Routing

import Utils.Path as UP

data RouterState = RouterState Routes Wiring (Driver Workspace.QueryP SlamDataRawEffects)

main ∷ Eff SlamDataEffects Unit
main = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.modePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.themePath (Config.baseUrl ⊕ "js/ace")
  runHalogenAff $ forkAff routeSignal
  StyleLoader.loadStyles

routeSignal ∷ Aff SlamDataEffects Unit
routeSignal = do
  runProcess (routeProducer $$ routeConsumer Nothing)

  where
  routeProducer = produce \emit →
    Routing.matches' UP.decodeURIPath routing \_ → emit ∘ Left

  routeConsumer state  = do
    new ← await
    case new, state of
      -- Initialize the Workspace component
      WorkspaceRoute path deckId action varMaps, Nothing → do
        permissionTokenHashes ← lift $ fromEff Permission.retrieveTokenHashes
        wiring ← lift $ Wiring.make path (toAccessType action) varMaps permissionTokenHashes
        mount wiring new

      -- Reload the page on path change or varMap change
      WorkspaceRoute path _ action _, Just (RouterState (WorkspaceRoute path' _ action' _) wiring _)
        | path ≠ path' || toAccessType action ≠ toAccessType action' →
        reload wiring new

      -- Transition Workspace
      WorkspaceRoute path deckId action varMaps, Just (RouterState old wiring driver) →
        case old of
          WorkspaceRoute _ deckId' _ varMaps'
            | deckId ≡ deckId' ∧ varMaps ≡ varMaps' →
                routeConsumer (Just (RouterState new wiring driver))
          WorkspaceRoute _ deckId' _ varMaps' → do
            when (varMaps ≠ varMaps') $ lift do
              liftEff $ writeRef (Wiring.unWiring wiring).varMaps varMaps
              diffVarMaps wiring varMaps' varMaps
            lift $ setup new driver
            routeConsumer (Just (RouterState new wiring driver))

  diffVarMaps (Wiring wiring) vm1 vm2 = do
    decks ← Cache.snapshot wiring.eval.decks
    cards ← Cache.snapshot wiring.eval.cards
    let
      vm1' = resolveUrlVarMaps decks cards vm1
      vm2' = resolveUrlVarMaps decks cards vm2
    runSlam (Wiring wiring)
      $ traverse_ (P.queueEvalImmediate ∘ Card.toAll)
      $ Diff.updated
      $ Diff.diff vm1' vm2'

  reload (Wiring wiring) new@(WorkspaceRoute path _ action varMaps) = do
    wiring' ←
      if path ≡ wiring.path
        then pure $ Wiring wiring { path = path, accessType = toAccessType action }
        else lift $ Wiring.make path (toAccessType action) varMaps wiring.auth.permissionTokenHashes
    lift $ removeFromBody ".sd-workspace"
    mount wiring' new

  mount wiring new@(WorkspaceRoute _ _ action _) = do
    let ui = interpret (runSlam wiring) $ Workspace.comp (toAccessType action)
    driver ← lift $ runUI ui (parentState Workspace.initialState) =<< awaitBody'
    lift $ setup new driver
    routeConsumer (Just (RouterState new wiring driver))

  setup new@(WorkspaceRoute _ deckId action varMaps) driver = do
    when (toAccessType action ≡ AT.ReadOnly) do
      isEmbedded ←
        liftEff $ Browser.detectEmbedding
      let
        bodyClass =
          if isEmbedded
            then "sd-workspace-page sd-embedded"
            else "sd-workspace-page"
      void
        $ liftEff
        $ traverse (setClassName bodyClass ∘ htmlElementToElement)
        ∘ toMaybe
        =<< body
        =<< document
        =<< window

    forkAff case action of
      Load _ →
        driver $ Workspace.toWorkspace $ Workspace.Load deckId
      New →
        driver $ Workspace.toWorkspace $ Workspace.New
      Exploring fp →
        driver $ Workspace.toWorkspace $ Workspace.ExploreFile fp

awaitBody' ∷ Aff SlamDataEffects HTMLElement
awaitBody' = do
  body ← liftEff $ map toMaybe $ window >>= document >>= body
  maybe awaitBody pure body

removeFromBody ∷ String → Aff SlamDataEffects Unit
removeFromBody sel = liftEff $ void $ runMaybeT do
  body ← MaybeT $ map toMaybe $ window >>= document >>= body
  root ← MaybeT $ map toMaybe $ window >>= document >>= htmlDocumentToParentNode >>> querySelector sel
  lift $ removeChild (elementToNode root) (htmlElementToNode body)
