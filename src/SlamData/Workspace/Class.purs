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

module SlamData.Workspace.Class
  ( class WorkspaceDSL
  , navigate
  , navigateToDeck
  , navigateToIndex
  , module SlamData.Workspace.Routing
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (REF, readRef)
import Control.UI.Browser as Browser

import Data.List as L

import DOM (DOM)

import Halogen.Query (HalogenM)

import SlamData.FileSystem.Routing (parentURL)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Routing (Routes(..))

class WorkspaceDSL (m ∷ * → *) where
  navigate ∷ Routes → m Unit

instance workspaceDSLMaybeT ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (MaybeT m) where
  navigate = lift ∘ navigate

instance workspaceDSLExceptT ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (ExceptT e m) where
  navigate = lift ∘ navigate

instance workspaceDSLHalogenM ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (HalogenM s f g p o m) where
  navigate = lift ∘ navigate

navigateToDeck
  ∷ ∀ m eff
  . ( MonadAsk Wiring m
    , MonadEff (ref ∷ REF, dom ∷ DOM | eff) m
    , WorkspaceDSL m
    )
  ⇒ L.List DeckId
  → m Unit
navigateToDeck = case _ of
  L.Nil → navigateToIndex
  cursor → do
    { path, accessType, varMaps } ← Wiring.expose
    urlVarMaps ← liftEff $ readRef varMaps
    navigate $ WorkspaceRoute path cursor (WA.Load accessType) urlVarMaps

navigateToIndex
  ∷ ∀ m eff
  . ( MonadAsk Wiring m
    , MonadEff (dom ∷ DOM | eff) m
    )
  ⇒ m Unit
navigateToIndex = do
  { path } ← Wiring.expose
  void $ liftEff $ Browser.setHref $ parentURL $ Left path
