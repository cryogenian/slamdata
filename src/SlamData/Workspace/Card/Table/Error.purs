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

module SlamData.Workspace.Card.Table.Error where

import SlamData.Prelude

import Quasar.QuasarF (QError)
import SlamData.GlobalError as GE
import Utils (hush)

data TableError
  = TableMissingResourceInputError
  | TableCountQuasarError QError
  | TableSampleQuasarError QError

instance showTableError ∷ Show TableError where
  show = case _ of
    TableMissingResourceInputError → "TableMissingResourceInputError"
    TableCountQuasarError err → "(TableCountQuasarError " <> show err <> ")"
    TableSampleQuasarError err → "(TableSampleQuasarError " <> show err <> ")"

tableToGlobalError ∷ TableError → Maybe GE.GlobalError
tableToGlobalError = case _ of
  TableCountQuasarError qErr → hush (GE.fromQError qErr)
  TableSampleQuasarError qErr → hush (GE.fromQError qErr)
  _ → Nothing
