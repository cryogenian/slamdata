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
import Test.StrongCheck (QC())
import Control.Monad.Eff.Console (log)

main :: QC Unit
main = do

  log "Check Data.SQL2.Literal..."
  Test.Property.SQL2.Literal.check

  log "Check Model.Resource..."
  Test.Property.Model.Resource.check

  log "Check Notebook.Cell.CellId..."
  Test.Property.Notebook.Cell.CellId.check

  log "Check Notebook.Cell.CellType..."
  Test.Property.Notebook.Cell.CellType.check

  log "Check Notebook.Cell.Model..."
  Test.Property.Notebook.Cell.Model.check

  log "Check Notebook.Cell.Chart.ChartType..."
  Test.Property.Notebook.Cell.Chart.ChartType.check

  log "Check Notebook.Cell.JTable.Model..."
  Test.Property.Notebook.Cell.JTable.Model.check

  log "Check Notebook.Cell.Markdown.Model..."
  Test.Property.Notebook.Cell.Markdown.Model.check

  log "Check Notebook.Cell.Search.Model..."
  Test.Property.Notebook.Cell.Search.Model.check

  log "Check Notebook.FormBuilder.Item.Model..."
  Test.Property.Notebook.FormBuilder.Item.Model.check

  log "Check Notebook.FormBuilder.Model..."
  Test.Property.Notebook.FormBuilder.Model.check

  log "Check Notebook.Model..."
  Test.Property.Notebook.Model.check

