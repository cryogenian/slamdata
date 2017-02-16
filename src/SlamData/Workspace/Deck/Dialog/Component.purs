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
  , Query(..)
  , Message(..)
  , ChildSlot
  , component
  ) where

import SlamData.Prelude

import Data.Map as Map

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import SlamData.Dialog.Error.Component as Error
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.InsertableCardType (InsertableCardType)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.Dialog.Confirm.Component as Confirm
import SlamData.Workspace.Deck.Dialog.Export.Component as Export
import SlamData.Workspace.Deck.Dialog.Reason.Component as Reason
import SlamData.Workspace.Deck.Dialog.Rename.Component as Rename
import SlamData.Workspace.Deck.Dialog.Share.Component as Share
import SlamData.Workspace.Deck.Dialog.Share.Model (SharingInput)
import SlamData.Workspace.Deck.Dialog.Unshare.Component as Unshare

data Dialog
  = Error String
  | Embed SharingInput (Map.Map CardId Port.VarMap)
  | Publish SharingInput (Map.Map CardId Port.VarMap)
  | Reason CardType String (Array (Array InsertableCardType))
  | Share SharingInput
  | Unshare SharingInput
  | Rename String
  | DeleteDeck

type State = Maybe Dialog

data Query a
  = Show Dialog a
  | Raise Message a

data Message
  = Dismiss
  | FlipToFront
  | Confirm Dialog Boolean
  | SetDeckName String

type ChildQuery =
  Rename.Query
  ⨁ Error.Query
  ⨁ Confirm.Query
  ⨁ Export.Query
  ⨁ Export.Query
  ⨁ Share.Query
  ⨁ Unshare.Query
  ⨁ Reason.Query
  ⨁ Const Void

type ChildSlot =
  Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam

type DSL = H.ParentDSL State Query ChildQuery ChildSlot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.parentComponent
    { render
    , eval
    , initialState: const Nothing
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ HH.ClassName "deck-dialog" ] ]
    $ foldMap (pure ∘ dialog) state
  where
  dialog = case _ of
    Rename name →
      HH.slot' CP.cp1 unit Rename.component name
        case _ of
          Rename.Dismiss → Just $ H.action $ Raise Dismiss
          Rename.Rename name' → Just $ H.action $ Raise (SetDeckName name')

    Error str →
      HH.slot' CP.cp2 unit Error.nonModalComponent str
        \Error.Dismiss → Just $ H.action $ Raise Dismiss

    DeleteDeck →
      HH.slot' CP.cp3 unit Confirm.component
        { title: "Delete deck"
        , body: "Are you sure you want to delete this deck?"
        , cancel: "Cancel"
        , confirm: "Delete"
        }
        \(Confirm.Confirm bool) → Just $ H.action $ Raise (Confirm DeleteDeck bool)

    Embed sharingInput varMaps →
      HH.slot' CP.cp4 unit Export.component
        { sharingInput
        , presentingAs: Export.IFrame
        , varMaps
        }
        \Export.Dismiss → Just $ H.action $ Raise Dismiss

    Publish sharingInput varMaps →
      HH.slot' CP.cp5 unit Export.component
        { sharingInput
        , presentingAs: Export.URI
        , varMaps
        }
        \Export.Dismiss → Just $ H.action $ Raise Dismiss

    Share sharingInput →
      HH.slot' CP.cp6 unit Share.component sharingInput
        \Share.Dismiss → Just $ H.action $ Raise Dismiss

    Unshare sharingInput →
      HH.slot' CP.cp7 unit Unshare.component sharingInput
        \Unshare.Dismiss → Just $ H.action $ Raise Dismiss

    Reason attemptedCardType reason cardPaths →
      HH.slot' CP.cp8 unit Reason.component
        { attemptedCardType
        , reason
        , cardPaths
        }
        \Reason.Dismiss → Just $ H.action $ Raise FlipToFront

eval ∷ Query ~> DSL
eval = case _ of
  Show dialog next → H.put (Just dialog) $> next
  Raise msg next → next <$ case msg of
    Dismiss → H.put Nothing *> H.raise msg
    _ → H.raise msg
