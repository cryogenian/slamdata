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

module Test.Property where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.StrongCheck (QC())

main :: QC Unit
main = do
  log "Check Data.SQL2.Literal..."
  Test.Property.Data.SQL2.Literal.check

  log "Check SlamData.FileSystem.Resource..."
  Test.Property.SlamData.FileSystem.Resource.check

  log "Check SlamData.Form.Select..."
  Test.Property.SlamData.Form.Select.check

  log "Check SlamData.Notebook.Cell.CellId..."
  Test.Property.SlamData.Notebook.Cell.CellId.check

  log "Check SlamData.Notebook.Cell.CellType..."
  Test.Property.SlamData.Notebook.Cell.CellType.check

  log "Check SlamData.Notebook.Cell.Model..."
  Test.Property.SlamData.Notebook.Cell.Model.check

  log "Check SlamData.Notebook.Cell.Chart.Aggregation..."
  Test.Property.SlamData.Notebook.Cell.Chart.Aggregation.check

  log "Check SlamData.Notebook.Cell.Chart.ChartConfiguration..."
  Test.Property.SlamData.Notebook.Cell.Chart.ChartConfiguration.check

  log "Check SlamData.Notebook.Cell.Chart.ChartType..."
  Test.Property.SlamData.Notebook.Cell.Chart.ChartType.check

  log "Check SlamData.Notebook.Cell.JTable.Model..."
  Test.Property.SlamData.Notebook.Cell.JTable.Model.check

  log "Check SlamData.Notebook.Cell.Markdown.Model..."
  Test.Property.SlamData.Notebook.Cell.Markdown.Model.check

  log "Check SlamData.Notebook.Cell.Search.Model..."
  Test.Property.SlamData.Notebook.Cell.Search.Model.check

  log "Check SlamData.Notebook.Cell.Viz.Model..."
  Test.Property.SlamData.Notebook.Cell.Viz.Model.check

  log "Check SlamData.Notebook.FormBuilder.Item.Model..."
  Test.Property.SlamData.Notebook.FormBuilder.Item.Model.check

  log "Check SlamData.Notebook.FormBuilder.Model..."
  Test.Property.SlamData.Notebook.FormBuilder.Model.check

  log "Check SlamData.Notebook.Model..."
  Test.Property.SlamData.Notebook.Model.check

  log "Check SlamData.Download.Model..."
  Test.Property.SlamData.Download.Model.check

  log "Check SlamData.Notebook.Cell.Download.Component.State..."
  Test.Property.SlamData.Notebook.Cell.Download.Component.State.check

  log "Check SlamData.Notebook.Cell.Ace.Model..."
  Test.Property.SlamData.Notebook.Cell.Ace.Model.check
