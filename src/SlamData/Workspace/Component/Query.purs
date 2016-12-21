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

module SlamData.Workspace.Component.Query where

import SlamData.Prelude

import Data.List (List)

import Halogen as H
import Halogen.HTML.Events.Types as HET

import Quasar.Advanced.Types (ProviderR)

import SlamData.Wiring (StepByStepGuide)
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Deck.DeckId (DeckId)

import Utils.Path as UP

data Query a
  = Init a
  | DismissAll (HET.Event HET.MouseEvent) a
  | New a
  | Load (List DeckId) a
  | ExploreFile UP.FilePath a
  | PresentStepByStepGuide StepByStepGuide a
  | CardGuideStepNext a
  | CardGuideDismiss a
  | FlipGuideStepNext a
  | FlipGuideDismiss a
  | Resize a
  | SignIn ProviderR a

type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)

toWorkspace ∷ H.Action Query → QueryP Unit
toWorkspace = left ∘ H.action

fromWorkspace ∷ ∀ a. (∀ i. (a → i) → Query i) → QueryP a
fromWorkspace r = left (H.request r)
