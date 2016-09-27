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

module SlamData.Workspace.Deck.Dialog.Component
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

import Data.Map as Map

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, (:>), cpL, cpR)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Component.Utils (raise')

import SlamData.Dialog.Error.Component as Error
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.InsertableCardType (InsertableCardType)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Dialog.Confirm.Component as Confirm
import SlamData.Workspace.Deck.Dialog.Export.Component as Export
import SlamData.Workspace.Deck.Dialog.Reason.Component as Reason
import SlamData.Workspace.Deck.Dialog.Rename.Component as Rename
import SlamData.Workspace.Deck.Dialog.Share.Component as Share
import SlamData.Workspace.Deck.Dialog.Share.Model (SharingInput)
import SlamData.Workspace.Deck.Dialog.Unshare.Component as Unshare

data Dialog
  = Error String
  | Embed SharingInput (Map.Map DeckId Port.VarMap)
  | Publish SharingInput (Map.Map DeckId Port.VarMap)
  | Reason CardType String (Array (Array InsertableCardType))
  | Share SharingInput
  | Unshare SharingInput
  | Rename String
  | DeleteDeck

type State = Maybe Dialog

initialState ∷ State
initialState = Nothing

data Query a
  = Dismiss a
  | FlipToFront a
  | Confirm Dialog Boolean a
  | SetDeckName String a
  | Show Dialog a

type ChildState =
  Rename.State
  ⊹ Error.State
  ⊹ Confirm.State
  ⊹ Export.State
  ⊹ Export.State
  ⊹ Share.State
  ⊹ Unshare.State
  ⊹ Reason.State

type ChildQuery =
  Rename.Query
  ⨁ Error.Query
  ⨁ Confirm.Query
  ⨁ Export.Query
  ⨁ Export.Query
  ⨁ Share.Query
  ⨁ Unshare.Query
  ⨁ Reason.Query

type ChildSlot =
  Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit

cpRename
  ∷ ChildPath
       Rename.State ChildState
       Rename.Query ChildQuery
       Unit ChildSlot
cpRename = cpL

cpError
  ∷ ChildPath
       Error.State ChildState
       Error.Query ChildQuery
       Unit ChildSlot
cpError = cpR :> cpL


cpDeleteDeck
  ∷ ChildPath
      Confirm.State ChildState
      Confirm.Query ChildQuery
      Unit ChildSlot
cpDeleteDeck = cpR :> cpR :> cpL

cpPublish
  ∷ ChildPath
      Export.State ChildState
      Export.Query ChildQuery
      Unit ChildSlot
cpPublish = cpR :> cpR :> cpR :> cpL

cpEmbed
  ∷ ChildPath
      Export.State ChildState
      Export.Query ChildQuery
      Unit ChildSlot
cpEmbed = cpR :> cpR :> cpR :> cpR :> cpL


cpShare
  ∷ ChildPath
      Share.State ChildState
      Share.Query ChildQuery
      Unit ChildSlot
cpShare = cpR :> cpR :> cpR :> cpR :> cpR :> cpL

cpUnshare
  ∷ ChildPath
      Unshare.State ChildState
      Unshare.Query ChildQuery
      Unit ChildSlot
cpUnshare = cpR :> cpR :> cpR :> cpR :> cpR :> cpR :> cpL

cpReason
  ∷ ChildPath
      Reason.State ChildState
      Reason.Query ChildQuery
      Unit ChildSlot
cpReason = cpR :> cpR :> cpR :> cpR :> cpR :> cpR :> cpR

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)

type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ HH.className "deck-dialog" ] ]
    $ foldMap (pure ∘ dialog) state
  where

  dialog (Rename name) =
    HH.slot' cpRename unit \_ →
      { component: Rename.comp
      , initialState: { newName: name }
      }

  dialog (Error str) =
    HH.slot' cpError unit \_ →
      { component: Error.nonModalComp
      , initialState: Error.State str
      }

  dialog (Embed sharingInput varMaps) =
    HH.slot' cpEmbed unit \_ →
      { component: Export.comp
      , initialState:
          (Export.initialState sharingInput)
            { presentingAs = Export.IFrame
            , varMaps = varMaps
            }
      }

  dialog DeleteDeck =
    HH.slot' cpDeleteDeck unit \_ →
      { component: Confirm.comp
      , initialState:
          { title: "Delete deck"
          , body: "Are you sure you want to delete this deck?"
          , cancel: "Cancel"
          , confirm: "Delete"
          }
      }
  dialog (Publish sharingInput varMaps) =
    HH.slot' cpPublish unit \_ →
      { component: Export.comp
      , initialState:
          (Export.initialState sharingInput)
            { presentingAs = Export.URI
            , varMaps = varMaps
            }
      }
  dialog (Share deckPath) =
    HH.slot' cpShare unit \_ →
      { component: Share.comp
      , initialState: Share.initialState deckPath
      }

  dialog (Unshare deckPath) =
    HH.slot' cpUnshare unit \_ →
      { component: Unshare.comp
      , initialState: Unshare.initialState deckPath
      }

  dialog (Reason attemptedCardType reason cardPaths) =
    HH.slot' cpReason unit \_ →
      { component: Reason.comp
      , initialState: { attemptedCardType, reason, cardPaths }
      }


eval ∷ Query ~> DSL
eval (Dismiss next) = H.set Nothing $> next
eval (FlipToFront next) = H.set Nothing $> next
eval (SetDeckName _ next) = H.set Nothing $> next
eval (Confirm _ _ next) = H.set Nothing $> next
eval (Show d next) = H.set (Just d) $> next

peek ∷ ∀ a. ChildQuery a → DSL Unit
peek =
  renamePeek
  ⨁ errorPeek
  ⨁ deleteDeckPeek
  ⨁ exportPeek
  ⨁ exportPeek
  ⨁ sharePeek
  ⨁ unsharePeek
  ⨁ reasonPeek

-- Send `Dismiss` after child's `Dismiss` to simplify parent of
-- this component peeking. (I.e. it can observe only this component queries and
-- don't provide separate handlers for embed dialog)
renamePeek ∷ ∀ a. Rename.Query a → DSL Unit
renamePeek (Rename.Dismiss _) = raise' $ Dismiss unit
renamePeek (Rename.Rename name _) = raise' $ SetDeckName name unit
renamePeek _ = pure unit

errorPeek ∷ ∀ a. Error.Query a → DSL Unit
errorPeek (Error.Dismiss _) =
  raise' $ Dismiss unit

deleteDeckPeek ∷ ∀ a. Confirm.Query a → DSL Unit
deleteDeckPeek (Confirm.Confirm b _) = do
  H.get >>= traverse_ \dialog →
    raise' $ Confirm dialog b unit

exportPeek ∷ ∀ a. Export.Query a → DSL Unit
exportPeek (Export.Dismiss _) =
  raise' $ Dismiss unit
exportPeek _ = pure unit

sharePeek ∷ ∀ a. Share.Query a → DSL Unit
sharePeek (Share.Dismiss _) =
  raise' $ Dismiss unit
sharePeek _ = pure unit

unsharePeek ∷ ∀ a. Unshare.Query a → DSL Unit
unsharePeek (Unshare.Dismiss _) =
  raise' $ Dismiss unit
unsharePeek _ = pure unit

reasonPeek ∷ ∀ a. Reason.Query a → DSL Unit
reasonPeek (Reason.Dismiss _) =
  raise' $ FlipToFront unit
