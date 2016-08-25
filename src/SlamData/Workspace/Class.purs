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

module SlamData.Workspace.Class where

import SlamData.Prelude

import Control.Monad.Free (Free, liftF)

import Data.Map as Map

import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenF as HF

import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Card.Port.VarMap as Port

class WorkspaceDSL m where
  getURLVarMaps ∷ m (Map.Map DeckId Port.URLVarMap)
  putURLVarMaps ∷ Map.Map DeckId Port.URLVarMap → m Unit

instance workspaceDSLMaybeT ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (MaybeT m) where
  getURLVarMaps = lift getURLVarMaps
  putURLVarMaps = lift ∘ putURLVarMaps

instance workspaceDSLExceptT ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (ExceptT e m) where
  getURLVarMaps = lift getURLVarMaps
  putURLVarMaps = lift ∘ putURLVarMaps

instance workspaceDSLFree ∷ WorkspaceDSL m ⇒ WorkspaceDSL (Free m) where
  getURLVarMaps = liftF getURLVarMaps
  putURLVarMaps = liftF ∘ putURLVarMaps

instance workspaceDSLHFC ∷ WorkspaceDSL g ⇒ WorkspaceDSL (HF.HalogenFP ES.EventSource s f g) where
  getURLVarMaps = HF.QueryHF getURLVarMaps
  putURLVarMaps = HF.QueryHF ∘ putURLVarMaps

instance workspaceDSLHFP ∷ WorkspaceDSL g ⇒ WorkspaceDSL (HF.HalogenFP ES.ParentEventSource s f (Free (HF.HalogenFP ES.EventSource s' f' g))) where
  getURLVarMaps = HF.QueryHF getURLVarMaps
  putURLVarMaps = HF.QueryHF ∘ putURLVarMaps
