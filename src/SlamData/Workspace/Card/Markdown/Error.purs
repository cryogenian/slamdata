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

module SlamData.Workspace.Card.Markdown.Error where

import SlamData.Prelude
import Data.List.NonEmpty as NEL
import Utils (throwVariantError)

data MarkdownError
  = MarkdownParseError { markdown ∷ String, error ∷ String }
  | MarkdownSqlParseError { field ∷ Maybe String, sql ∷ String, error ∷ String }
  | MarkdownNoTextBoxResults { field ∷ String, sql ∷ String }
  | MarkdownInvalidTimeValue { field ∷ String, time ∷ String, error ∷ String }
  | MarkdownInvalidDateValue { field ∷ String, date ∷ String, error ∷ String }
  | MarkdownInvalidDateTimeValue { field ∷ String, datetime ∷ String, error ∷ String }
  | MarkdownTypeError { field ∷ String, actual ∷ String, expected ∷ NEL.NonEmptyList String }

instance showMarkdownError ∷ Show MarkdownError where
  show = case _ of
    MarkdownParseError {markdown, error} →
      "(MarkdownParseError { markdown: " <> show markdown <> ", error: " <> show error <> " })"
    MarkdownSqlParseError {field, sql, error} →
      "(MarkdownSqlParseError { field: " <> show field <> ", sql: " <> show sql <> ", error: " <> show error <> " })"
    MarkdownNoTextBoxResults {field, sql} →
      "(MarkdownNoTextBoxResults { field: " <> show field <> ", sql: " <> show sql <> " })"
    MarkdownInvalidTimeValue {field, time, error} →
      "(MarkdownInvalidTimeValue { field: " <> show field <> ", time: " <> show time <> ", error: " <> show error <> " })"
    MarkdownInvalidDateValue {field, date, error} →
      "(MarkdownInvalidDateValue { field: " <> show field <> ", date: " <> show date <> ", error: " <> show error <> " })"
    MarkdownInvalidDateTimeValue {field, datetime, error} →
      "(MarkdownInvalidDateTimeValue { field: " <> show field <> ", datetime: " <> show datetime <> ", error: " <> show error <> " })"
    MarkdownTypeError {field, actual, expected} →
      "(MarkdownTypeError { field: " <> show field <> ", actual: " <> show actual <> ", expected: " <> show expected <> " })"

throwMarkdownError ∷ forall v m a. MonadThrow (Variant (markdown ∷ MarkdownError | v)) m ⇒ MarkdownError → m a
throwMarkdownError = throwVariantError (SProxy :: SProxy "markdown")
