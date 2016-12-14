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

module SlamData.Wiring
  ( Wiring(..)
  , WiringR
  , EvalWiring
  , AuthWiring
  , CacheWiring
  , BusWiring
  , DeckMessage(..)
  , StepByStepGuide(..)
  , ActiveState
  , PendingEval
  , PendingSave
  , make
  , unWiring
  , expose
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable, fromAff, fromEff)
import Control.Monad.Eff.Ref (Ref, newRef)

import Data.Map as Map

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Notification as N
import SlamData.Quasar.Auth.Authentication as Auth
import SlamData.AuthenticationMode (AllowedAuthenticationModes, allowedAuthenticationModesForAccessType)
import SlamData.GlobalMenu.Bus (SignInBus)
import SlamData.Workspace.AccessType (AccessType)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Workspace.Eval.Graph (EvalGraph)
import SlamData.Wiring.Cache (Cache)
import SlamData.Wiring.Cache as Cache

import Utils.Path (DirPath)

data DeckMessage
  = DeckFocused DeckId

data StepByStepGuide
  = CardGuide
  | FlipGuide

type ActiveState =
  { cardIndex ∷ Int
  }

type PendingEval =
  { source ∷ Card.DisplayCoord
  , graph ∷ EvalGraph
  , avar ∷ AVar Unit
  }

type PendingSave =
  { avar ∷ AVar Unit
  }

type EvalWiring =
  { tick ∷ Ref Int
  , cards ∷ Cache Card.Coord Card.Cell
  , decks ∷ Cache Deck.Id Deck.Cell
  -- We need to use AVars for debounce state rather than storing Cancelers,
  -- because the Canceler would need to reference `Slam` resulting in a
  -- circular dependency.
  , pendingEvals ∷ Cache Card.Coord PendingEval
  , pendingSaves ∷ Cache Deck.Id PendingSave
  }

type AuthWiring =
  { hasIdentified ∷ Ref Boolean
  , requestToken ∷ Auth.RequestIdTokenBus
  , signIn ∷ SignInBus
  , allowedModes ∷ AllowedAuthenticationModes
  }

type CacheWiring =
  { activeState ∷ Cache Deck.Id ActiveState
  }

type BusWiring =
  { decks ∷ Bus.BusRW DeckMessage
  , notify ∷ Bus.BusRW N.NotificationOptions
  , globalError ∷ Bus.BusRW GE.GlobalError
  , stepByStep ∷ Bus.BusRW StepByStepGuide
  }

type WiringR =
  { path ∷ DirPath
  , accessType ∷ AccessType
  , varMaps ∷ Cache Deck.Id Port.URLVarMap
  , eval ∷ EvalWiring
  , auth ∷ AuthWiring
  , cache ∷ CacheWiring
  , bus ∷ BusWiring
  }

newtype Wiring = Wiring WiringR

unWiring ∷ Wiring → WiringR
unWiring (Wiring w) = w

expose
  ∷ ∀ m
  . (MonadAsk Wiring m)
  ⇒ m WiringR
expose = unWiring <$> ask

make
  ∷ ∀ m
  . (Affable SlamDataEffects m)
  ⇒ DirPath
  → AccessType
  → Map.Map Deck.Id Port.URLVarMap
  → m Wiring
make path accessType vm = fromAff do
  eval ← makeEval
  auth ← makeAuth
  cache ← makeCache
  bus ← makeBus
  varMaps ← Cache.make' vm
  pure $ Wiring { path, accessType, varMaps, eval, auth, cache, bus }

  where
  makeEval = do
    tick ← fromEff (newRef 0)
    cards ← Cache.make
    decks ← Cache.make
    pendingEvals ← Cache.make
    pendingSaves ← Cache.make
    pure { tick, cards, decks, pendingEvals, pendingSaves }

  makeAuth = do
    hasIdentified ← fromEff (newRef false)
    requestToken ← Auth.authentication
    signIn ← Bus.make
    let allowedModes = allowedAuthenticationModesForAccessType accessType
    pure { hasIdentified, requestToken, signIn, allowedModes }

  makeCache = do
    activeState ← Cache.make
    pure { activeState }

  makeBus = do
    decks ← Bus.make
    notify ← Bus.make
    globalError ← Bus.make
    stepByStep ← Bus.make
    pure { decks, notify, globalError, stepByStep }
