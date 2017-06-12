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

module SlamData.Workspace.Card.Search.Eval
  ( evalSearch
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Writer.Class (class MonadTell)
import Control.Monad.Throw (rethrow)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class ParQuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Search.Interpret as Search
import SqlSquared as Sql
import Text.SlamSearch as SS

evalSearch
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ ParQuasarDSL m
  ⇒ String
  → Port.Port
  → m Port.Out
evalSearch queryText port = do
  let
    queryText'
      | queryText ≡ "" = "*"
      | otherwise = queryText
  CEM.CardEnv { cardId, varMap } ← ask
  searchQuery ← case SS.mkQuery queryText' of
    Left pe → CE.throwSearchError $ CE.SearchQueryParseError { query: queryText, error: pe }
    Right q → pure q
  resourceVar ← CEM.extractResourceVar port
  let
    sql = Search.searchSql resourceVar (VM.Var Search.defaultFilterVar)
    filter = VM.Expr $ Search.filterSql mempty searchQuery
    varMap' = VM.insert cardId (VM.Var Search.defaultFilterVar) filter varMap
  resource ← CEC.localEvalResource (Sql.Query mempty sql) varMap >>= searchError CE.SearchQueryCompilationError
  pure $ Port.resourceOut cardId resource varMap

searchError ∷ ∀ e a m. MonadThrow CE.CardError m ⇒ (e → CE.SearchError) → Either e a → m a
searchError f = rethrow ∘ lmap (CE.SearchCardError ∘ f)
