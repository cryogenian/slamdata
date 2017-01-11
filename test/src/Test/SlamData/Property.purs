{-
Copyright 2015 SlamData, Inc.

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

module Test.SlamData.Property where

import Prelude

import Control.Monad.Eff.Console (log)

{-import Test.SlamData.Property.Download.Model as Test.SlamData.Property.Download.Model
import Test.SlamData.Property.FileSystem.Resource as Test.SlamData.Property.FileSystem.Resource
import Test.SlamData.Property.Form.Select as Test.SlamData.Property.Form.Select
import Test.SlamData.Property.Workspace.Card.Ace.Model as Test.SlamData.Property.Workspace.Card.Ace.Model
import Test.SlamData.Property.Workspace.Card.CardId as Test.SlamData.Property.Workspace.Card.CardId
import Test.SlamData.Property.Workspace.Card.CardType as Test.SlamData.Property.Workspace.Card.CardType
import Test.SlamData.Property.Workspace.Card.DownloadOptions.Component.State as Test.SlamData.Property.Workspace.Card.DownloadOptions.Component.State
import Test.SlamData.Property.Workspace.Card.Markdown.Model as Test.SlamData.Property.Workspace.Card.Markdown.Model
import Test.SlamData.Property.Workspace.Card.Model as Test.SlamData.Property.Workspace.Card.Model
import Test.SlamData.Property.Workspace.Card.Table.Model as Test.SlamData.Property.Workspace.Card.Table.Model
import Test.SlamData.Property.Workspace.Deck.DeckId as Test.SlamData.Property.Workspace.Deck.DeckId
import Test.SlamData.Property.Workspace.Deck.Model as Test.SlamData.Property.Workspace.Deck.Model
import Test.SlamData.Property.Workspace.FormBuilder.Item.Model as Test.SlamData.Property.Workspace.FormBuilder.Item.Model
import Test.SlamData.Property.Workspace.FormBuilder.Model as Test.SlamData.Property.Workspace.FormBuilder.Model

-}

import Test.SlamData.Property.Workspace.Deck.Dialog.Share.Model as ShareModel

import Test.StrongCheck (SC)

main ∷ ∀ eff. SC eff Unit
main = do
  log "Started..."
  ShareModel.check
  log "TROLOLO"
{-
main ∷ ∀ eff. SC eff Unit
main = do

  log "Check SlamData.Download.Model..."
  Test.SlamData.Property.Download.Model.check

  log "Check SlamData.FileSystem.Resource..."
  Test.SlamData.Property.FileSystem.Resource.check

  log "Check SlamData.Form.Select..."
  Test.SlamData.Property.Form.Select.check

  log "Check SlamData.Workspace.Card.CardId..."
  Test.SlamData.Property.Workspace.Card.CardId.check

  log "Check SlamData.Workspace.Card.CardType..."
  Test.SlamData.Property.Workspace.Card.CardType.check

  log "Check SlamData.Workspace.Card.Model..."
  Test.SlamData.Property.Workspace.Card.Model.check

  log "Check SlamData.Workspace.Card.Ace.Model..."
  Test.SlamData.Property.Workspace.Card.Ace.Model.check

  log "Check SlamData.Workspace.Card.DownloadOptions.Component.State..."
  Test.SlamData.Property.Workspace.Card.DownloadOptions.Component.State.check

  log "Check SlamData.Workspace.Card.Table.Model..."
  Test.SlamData.Property.Workspace.Card.Table.Model.check

  log "Check SlamData.Workspace.Card.Markdown.Model..."
  Test.SlamData.Property.Workspace.Card.Markdown.Model.check

  log "Check SlamData.Workspace.Deck.DeckId..."
  Test.SlamData.Property.Workspace.Deck.DeckId.check

  log "Check SlamData.Workspace.Deck.Model..."
  Test.SlamData.Property.Workspace.Deck.Model.check

  log "Check SlamData.Workspace.FormBuilder.Item.Model..."
  Test.SlamData.Property.Workspace.FormBuilder.Item.Model.check

  log "Check SlamData.Workspace.FormBuilder.Model..."
  Test.SlamData.Property.Workspace.FormBuilder.Model.check
-}
