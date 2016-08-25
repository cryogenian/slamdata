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

module SlamData.Analytics
  ( enableAnalytics
  , consumeEvents
  , identify
  , trackEvent
  , module SlamData.Analytics.Class
  , module SlamData.Analytics.Event
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)

import Data.Argonaut (Json, (:=), (~>), jsonEmptyObject)

import DOM (DOM)

import Network.HTTP.Affjax as AX

import OIDC.Crypt.Types as OIDC

import Quasar.QuasarF as QF
import Quasar.QuasarF.Interpreter.Aff as QA

import SlamData.Analytics.Class (class AnalyticsDSL, track)
import SlamData.Analytics.Event (Event(..))
import SlamData.Config as Config
import SlamData.Workspace.AccessType as AT

-- | Enables the segment.io analyics API.
foreign import enableAnalytics ∷ ∀ eff. Eff (dom ∷ DOM | eff) Unit

foreign import _track ∷ ∀ eff. String → Json → Eff (dom ∷ DOM | eff) Unit

isAdvanced ∷ ∀ eff. Aff (ajax ∷ AX.AJAX | eff) Boolean
isAdvanced
  = flip runReaderT { basePath: Config.baseUrl }
  $ QA.eval
  $ either (const false) (\{ name } → name == "Quasar-Advanced")
  <$> QF.serverInfo

-- | Identifies a user in the segment.io analytics API. This will have no effect
-- | if `enableAnalytics` has not previously been run.
foreign import identify ∷ ∀ eff. OIDC.Email → Eff (dom ∷ DOM | eff) Unit

consumeEvents
  ∷ ∀ r eff
  . Bus.Bus (read ∷ Bus.Cap | r) Event
  → Aff (avar ∷ AVAR, dom ∷ DOM | eff) Unit
consumeEvents bus = forever $ liftEff <<< trackEvent =<< Bus.read bus

trackEvent ∷ ∀ eff. Event → Eff (dom ∷ DOM | eff) Unit
trackEvent = case _ of
  AddCard cardType →
    _track "card-add" $ "cardType" := cardType ~> jsonEmptyObject
  Publish deckId →
    _track "deck-publish" $ "deckId" := deckId ~> jsonEmptyObject
  Embed deckId →
    _track "deck-embed" $ "deckId" := deckId ~> jsonEmptyObject
  Mirror deckId →
    _track "deck-mirror" $ "deckId" := deckId ~> jsonEmptyObject
  Wrap deckId →
    _track "deck-wrap" $ "deckId" := deckId ~> jsonEmptyObject
  Collapse deckId →
    _track "deck-collapse" $ "deckId" := deckId ~> jsonEmptyObject
  Delete deckId →
    _track "deck-delete" $ "deckId" := deckId ~> jsonEmptyObject
  Load deckId accessType →
    _track "workspace-load"
      $ "deckId" := deckId
      ~> "mode" := AT.printAccessType accessType
      ~> jsonEmptyObject
  Explore →
    _track "workspace-explore" jsonEmptyObject
  ErrorLoadingDeck →
    _track "error-deck-loading" jsonEmptyObject
  ErrorSavingDeck →
    _track "error-deck-saving" jsonEmptyObject
  ErrorSavingMirror →
    _track "error-deck-mirror-saving" jsonEmptyObject
  ErrorUpdatingRoot →
    _track "error-workspace-updating-root" jsonEmptyObject
  ErrorDeletingDeck →
    _track "error-deck-delete" jsonEmptyObject
  ErrorInCardEval cardType →
    _track "error-card-eval" $ "fromCard" := cardType ~> jsonEmptyObject
