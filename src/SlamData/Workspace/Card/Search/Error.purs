{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Search.Error where

import SlamData.Prelude

import Quasar.Advanced.QuasarAF (QError)
import SlamData.GlobalError as GE
import Text.Parsing.Parser (ParseError)
import Utils (throwVariantError, hush)

data SearchError
  = SearchQueryParseError { query ∷ String, error ∷ ParseError }
  | SearchQueryCompilationError QError

instance showSearchError ∷ Show SearchError where
  show = case _ of
    SearchQueryParseError { query, error } →
      "(SearchQueryParseError { query: " <> show query <> ", error: " <> show error <> " })"
    SearchQueryCompilationError error →
      "(SearchQueryCompilationError " <> show error <> ")"

searchToGlobalError ∷ SearchError → Maybe GE.GlobalError
searchToGlobalError = case _ of
  SearchQueryCompilationError error → hush (GE.fromQError error)
  _ → Nothing

throwSearchError ∷ forall v m a. MonadThrow (Variant (search ∷ SearchError | v)) m ⇒ SearchError → m a
throwSearchError = throwVariantError (SProxy :: SProxy "search")
