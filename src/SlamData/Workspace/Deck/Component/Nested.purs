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

module SlamData.Workspace.Deck.Component.Nested
  ( comp
  , module DNQ
  , module DNS
  ) where

import SlamData.Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.AVar (makeVar, putVar, takeVar)
import Control.Monad.Eff (Eff)

import Data.List (length)

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaque, opaqueQuery)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Query.EventSource as HE

import SlamData.Effects (SlamDataEffects)
import SlamData.Monad (Slam, runSlam)
import SlamData.Workspace.Deck.Common (DeckOptions)
import SlamData.Workspace.Deck.Component as DC
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Deck.Component.Nested.State as DNS
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State as DCS

type DSL = H.ComponentDSL DNS.State DNQ.QueryP Slam
type HTML = H.ComponentHTML DNQ.QueryP

comp ∷ DeckOptions → H.Component DNS.State DNQ.QueryP Slam
comp opts =
  H.lifecycleComponent
    { render
    , eval: coproduct eval evalProxy
    , initializer: Just (left $ H.action DNQ.Init)
    , finalizer: Just (left $ H.action DNQ.Finish)
    }

  where
  deckComponent' ∷ (DCQ.Query Unit → Eff SlamDataEffects Unit) → H.Component DCS.StateP DCQ.QueryP Slam
  deckComponent' emitter =
    opaque $ H.lifecycleParentComponent
      { render: DC.render opts comp
      , eval: \query → do
          res ← DC.eval opts query
          case query of
            DCQ.GrabDeck a _ → H.fromEff $ emitter $ DCQ.GrabDeck a unit
            _ → pure unit
          pure res
      , peek: Just (DC.peek opts)
      , initializer: Just (H.action DCQ.Init)
      , finalizer: Nothing
      }

  render ∷ DNS.State → HTML
  render _ =
    HH.div
      [ HP.classes
          [ HH.className "sd-deck-nested"
          , HH.className ("sd-deck-level-" <> show (length opts.displayCursor + 1))
          ]
      , HP.ref (left ∘ H.action ∘ DNQ.Ref)
      ]
      []

  eval ∷ DNQ.Query ~> DSL
  eval = case _ of
    DNQ.Init next → do
      el ← unsafePartial fromJust <$> H.gets _.el
      emitter ← H.fromAff makeVar
      H.subscribe $
        HE.EventSource $
          HE.produce \emit →
            void $ runAff (const (pure unit)) (const (pure unit)) $
              putVar emitter (emit <<< Left)
      emitter' ← H.fromAff $ takeVar emitter
      wiring ← H.liftH ask
      let ui = H.interpret (runSlam wiring) $ deckComponent' (emitter' ∘ right)
      driver ← H.fromAff $ H.runUI ui DC.initialState el
      H.modify _ { driver = Just (DNS.Driver driver) }
      pure next
    DNQ.Finish next → do
      DNS.Driver driver ← unsafePartial fromJust <$> H.gets _.driver
      H.fromAff $ driver $ opaqueQuery $ DCQ.Finish next
    DNQ.Ref el next → do
      H.modify _ { el = el }
      pure next

  evalProxy ∷ DCQ.Query ~> DSL
  evalProxy = case _ of
    -- These are the actions that are peeked, so they are terminal.
    DCQ.GrabDeck _ next → pure next
    -- The rest we'll just pass through
    query → do
      DNS.Driver driver ← unsafePartial fromJust <$> H.gets _.driver
      H.fromAff $ driver $ opaqueQuery query
