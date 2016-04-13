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

module SlamData.Notebook.Component.Query where

import SlamData.Prelude

import DOM.Event.EventTarget (EventListener)

import Halogen as H
import Halogen.Component.ChildPath (injSlot, injQuery)

import SlamData.Effects (SlamDataEffects)
import SlamData.Notebook.AccessType (AccessType)
import SlamData.Notebook.Cell.CellId (CellId)
import SlamData.Notebook.Component.ChildSlot (ChildQuery, ChildSlot, cpRename, cpDeck)
import SlamData.Notebook.Deck.Component.Query as Deck
import SlamData.Notebook.Menu.Component.Query as Menu
import SlamData.Notebook.Rename.Component as Rename

data Query a
  = ActivateKeyboardShortcuts a
  | DeactivateKeyboardShortcuts a
  | EvaluateMenuValue Menu.Value a
  | AddKeyboardListener (EventListener SlamDataEffects) a
  | SetAccessType AccessType a
  | GetAccessType (AccessType → a)
  | SetViewingCell (Maybe CellId) a
  | GetViewingCell (Maybe CellId → a)
  | SetParentHref String a
  | DismissAll a

type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)

toDraftboard ∷ H.Action Query → QueryP Unit
toDraftboard = left ∘ H.action

fromDraftboard ∷ ∀ a. (∀ i. (a → i) → Query i) → QueryP a
fromDraftboard r = left (H.request r)

toDeck ∷ H.Action Deck.Query → QueryP Unit
toDeck =
  right
    ∘ H.ChildF (injSlot cpDeck unit)
    ∘ injQuery cpDeck
    ∘ left
    ∘ H.action

fromDeck ∷ ∀ a. (∀ i. (a → i) → Deck.Query i) → QueryP a
fromDeck r =
  right
    $ H.ChildF (injSlot cpDeck unit)
    $ injQuery cpDeck
    $ left
    $ H.request r

toRename ∷ H.Action Rename.Query → QueryP Unit
toRename =
  right
    ∘ H.ChildF (injSlot cpRename unit)
    ∘ injQuery cpRename
    ∘ H.action

fromRename ∷ ∀ a. (∀ i. (a → i) → Rename.Query i) → QueryP a
fromRename r =
  right
    $ H.ChildF (injSlot cpRename unit)
    $ injQuery cpRename
    $ H.request r
