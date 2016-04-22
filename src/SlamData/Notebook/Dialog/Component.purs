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

module SlamData.Notebook.Dialog.Component
  ( Dialog(..)
  , State
  , initialState
  , Query(..)
  , ChildSlot
  , ChildQuery
  , ChildState
  , StateP
  , QueryP
  , comp
  ) where

import SlamData.Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3, coproduct3)

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, (:>), cpL, cpR)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Error.Component as Error
import SlamData.Notebook.Card.Port.VarMap as Port
import SlamData.Notebook.Dialog.Embed.Component as Embed
import SlamData.FileSystem.Dialog.Share.Component as Share
import SlamData.Effects (Slam)
import SlamData.Render.Common (fadeWhen)

data Dialog
  = Error String
  | Embed String Port.VarMap
  | Share String

type State = Maybe Dialog

initialState ∷ State
initialState = Nothing

data Query a
  = Dismiss a
  | Show Dialog a

type ChildState =
  Either3
    Error.State
    Embed.State
    Share.State

type ChildQuery =
  Coproduct3
    Error.Query
    Embed.Query
    Share.Query

type ChildSlot =
  Either3
    Unit
    Unit
    Unit

cpError
  ∷ ChildPath
       Error.State ChildState
       Error.Query ChildQuery
       Unit ChildSlot
cpError =
  cpL :> cpL

cpEmbed
  ∷ ChildPath
       Embed.State ChildState
       Embed.Query ChildQuery
       Unit ChildSlot
cpEmbed =
  cpL :> cpR

cpShare
  ∷ ChildPath
      Share.State ChildState
      Share.Query ChildQuery
      Unit ChildSlot
cpShare =
  cpR

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)

type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes ([B.modal] ⊕ fadeWhen (isNothing state))
    , HE.onClick (HE.input_ Dismiss)
    ]
    (maybe [] (pure ∘ dialog) state)
  where

  dialog (Error str) =
    HH.slot' cpError unit \_ →
      { component: Error.comp
      , initialState: Error.State str
      }

  dialog (Embed url varMap) =
    HH.slot' cpEmbed unit \_ →
      { component: Embed.comp
      , initialState: { url, varMap }
      }

  dialog (Share str) =
    HH.slot' cpShare unit \_ →
      { component: Share.comp
      , initialState: Share.State str
      }

eval ∷ Natural Query DSL
eval (Dismiss next) = H.set Nothing $> next
eval (Show d next) = H.set (Just d) $> next

peek ∷ ∀ a. ChildQuery a → DSL Unit
peek =
  coproduct3
    errorPeek
    embedPeek
    sharePeek

errorPeek ∷ ∀ a. Error.Query a → DSL Unit
errorPeek (Error.Dismiss _) = H.set Nothing

embedPeek ∷ ∀ a. Embed.Query a → DSL Unit
embedPeek (Embed.Dismiss _) = H.set Nothing
embedPeek _ = pure unit

sharePeek ∷ ∀ a. Share.Query a → DSL Unit
sharePeek (Share.Dismiss _) = H.set Nothing
sharePeek _ = pure unit
