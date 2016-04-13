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

  log "Check SlamData.Notebook.Cell.CellId..."
  Test.SlamData.Property.Notebook.Cell.CellId.check

  log "Check SlamData.Notebook.Cell.CellType..."
  Test.SlamData.Property.Notebook.Cell.CellType.check

  log "Check SlamData.Notebook.Cell.Model..."
  Test.SlamData.Property.Notebook.Cell.Model.check

  log "Check SlamData.Notebook.Cell.Chart.Aggregation..."
  Test.SlamData.Property.Notebook.Cell.Chart.Aggregation.check

  log "Check SlamData.Notebook.Cell.Chart.ChartConfiguration..."
  Test.SlamData.Property.Notebook.Cell.Chart.ChartConfiguration.check

  log "Check SlamData.Notebook.Cell.Chart.ChartType..."
  Test.SlamData.Property.Notebook.Cell.Chart.ChartType.check

  log "Check SlamData.Notebook.Cell.JTable.Model..."
  Test.SlamData.Property.Notebook.Cell.JTable.Model.check

  log "Check SlamData.Notebook.Cell.Markdown.Model..."
  Test.SlamData.Property.Notebook.Cell.Markdown.Model.check

  log "Check SlamData.Notebook.Cell.Viz.Model..."
  Test.SlamData.Property.Notebook.Cell.Viz.Model.check

  log "Check SlamData.Notebook.FormBuilder.Item.Model..."
  Test.SlamData.Property.Notebook.FormBuilder.Item.Model.check

  log "Check SlamData.Notebook.FormBuilder.Model..."
  Test.SlamData.Property.Notebook.FormBuilder.Model.check

  log "Check SlamData.Notebook.Deck.Model..."
  Test.SlamData.Property.Notebook.Deck.Model.check

  log "Check SlamData.Download.Model..."
  Test.SlamData.Property.Download.Model.check

  log "Check SlamData.Notebook.Cell.Download.Component.State..."
  Test.SlamData.Property.Notebook.Cell.Download.Component.State.check

  log "Check SlamData.Notebook.Cell.Ace.Model..."
  Test.SlamData.Property.Notebook.Cell.Ace.Model.check
