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
  , module SlamData.Workspace.Routing
  ) where

import SlamData.Prelude

import Control.Monad.Free (Free, liftF)

import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenF as HF

import SlamData.Workspace.Routing (Routes(..))

class WorkspaceDSL (m ∷ * → *) where
  navigate ∷ Routes → m Unit

instance workspaceDSLMaybeT ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (MaybeT m) where
  navigate = lift ∘ navigate

instance workspaceDSLExceptT ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (ExceptT e m) where
  navigate = lift ∘ navigate

instance workspaceDSLFree ∷ WorkspaceDSL m ⇒ WorkspaceDSL (Free m) where
  navigate = liftF ∘ navigate

instance workspaceDSLHFC ∷ WorkspaceDSL g ⇒ WorkspaceDSL (HF.HalogenFP ES.EventSource s f g) where
  navigate = HF.QueryHF ∘ navigate

instance workspaceDSLHFP ∷ WorkspaceDSL g ⇒ WorkspaceDSL (HF.HalogenFP ES.ParentEventSource s f (Free (HF.HalogenFP ES.EventSource s' f' g))) where
  navigate = HF.QueryHF ∘ navigate
