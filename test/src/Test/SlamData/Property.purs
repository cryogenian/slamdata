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
import Test.StrongCheck (QC)

main :: QC Unit
main = do
  log "Check Data.SQL2.Literal..."
  Test.Property.Data.SQL2.Literal.check

  log "Check SlamData.FileSystem.Resource..."
  Test.SlamData.Property.FileSystem.Resource.check

  log "Check SlamData.Form.Select..."
  Test.SlamData.Property.Form.Select.check

  log "Check SlamData.Notebook.Card.CardId..."
  Test.SlamData.Property.Notebook.Card.CardId.check

  log "Check SlamData.Notebook.Card.CardType..."
  Test.SlamData.Property.Notebook.Card.CardType.check

  log "Check SlamData.Notebook.Card.Model..."
  Test.SlamData.Property.Notebook.Card.Model.check

  log "Check SlamData.Notebook.Card.Chart.Aggregation..."
  Test.SlamData.Property.Notebook.Card.Chart.Aggregation.check

  log "Check SlamData.Notebook.Card.Chart.ChartConfiguration..."
  Test.SlamData.Property.Notebook.Card.Chart.ChartConfiguration.check

  log "Check SlamData.Notebook.Card.Chart.ChartType..."
  Test.SlamData.Property.Notebook.Card.Chart.ChartType.check

  log "Check SlamData.Notebook.Card.JTable.Model..."
  Test.SlamData.Property.Notebook.Card.JTable.Model.check

  log "Check SlamData.Notebook.Card.Markdown.Model..."
  Test.SlamData.Property.Notebook.Card.Markdown.Model.check

  log "Check SlamData.Notebook.Card.Viz.Model..."
  Test.SlamData.Property.Notebook.Card.Viz.Model.check

  log "Check SlamData.Notebook.FormBuilder.Item.Model..."
  Test.SlamData.Property.Notebook.FormBuilder.Item.Model.check

  log "Check SlamData.Notebook.FormBuilder.Model..."
  Test.SlamData.Property.Notebook.FormBuilder.Model.check

  log "Check SlamData.Notebook.Deck.Model..."
  Test.SlamData.Property.Notebook.Deck.Model.check

  log "Check SlamData.Download.Model..."
  Test.SlamData.Property.Download.Model.check

  log "Check SlamData.Notebook.Card.Download.Component.State..."
  Test.SlamData.Property.Notebook.Card.Download.Component.State.check

  log "Check SlamData.Notebook.Card.Ace.Model..."
  Test.SlamData.Property.Notebook.Card.Ace.Model.check
