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

data MarkdownError
  = MarkdownParseError {markdown ∷ String, error ∷ String}
  | MarkdownSqlParseError {sql ∷ String, error ∷ String}
  | MarkdownNoTextBoxResults
  | MarkdownInvalidTimeValue {time ∷ String, error ∷ String}
  | MarkdownInvalidDateValue {date ∷ String, error ∷ String}
  | MarkdownInvalidDateTimeValue {datetime ∷ String, error ∷ String}
  | MarkdownTypeError String String

instance showMarkdownError ∷ Show MarkdownError where
  show = case _ of
    MarkdownParseError {markdown, error} →
      "(MarkdownParseError { markdown: " <> show markdown <> ", error: " <> show error <> " })"
    MarkdownSqlParseError {sql, error} →
      "(MarkdownSqlParseError { sql: " <> show sql <> ", error: " <> show error <> " })"
    MarkdownNoTextBoxResults → "MarkdownNoTextBoxResults"
    MarkdownInvalidTimeValue {time, error} →
      "(MarkdownInvalidTimeValue { time: " <> show time <> ", error: " <> show error <> " })"
    MarkdownInvalidDateValue {date, error} →
      "(MarkdownInvalidDateValue { date: " <> show date <> ", error: " <> show error <> " })"
    MarkdownInvalidDateTimeValue {datetime, error} →
      "(MarkdownInvalidDateTimeValue { datetime: " <> show datetime <> ", error: " <> show error <> " })"
    MarkdownTypeError t1 t2 →
      "(MarkdownTypeError " <> show t1 <> " " <> show t2 <> ")"
