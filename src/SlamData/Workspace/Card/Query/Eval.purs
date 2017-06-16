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

module SlamData.Workspace.Card.Query.Eval
  ( evalQuery
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Path.Pathy as Path
import Quasar.Advanced.QuasarAF as QF
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common (validateResources)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Query.Error (QueryError(..), throwQueryError)
import SqlSquared as Sql

evalQuery
  ∷ ∀ m v
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow (Variant (query ∷ QueryError, qerror ∷ CE.QError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ String
  → Port.DataMap
  → m Port.Out
evalQuery sql varMap = do
  resource ← CEM.temporaryOutputResource
  let
    varMap' = Sql.print ∘ unwrap <$> Port.flattenResources varMap
    backendPath = fromMaybe Path.rootDir (Path.parentDir resource)
  { inputs } ← QQ.compile' backendPath sql varMap' >>= queryError QueryCompileError
  validateResources inputs
  CEM.addSources inputs
  QQ.viewQuery' resource sql varMap' >>= queryError QueryRetrieveResultError
  QQ.liftQuasar (QF.fileMetadata resource) >>= queryError QueryRetrieveResultError
  pure $ Port.resourceOut $ Port.View resource sql varMap

queryError ∷ ∀ e a m v. MonadThrow (Variant (query ∷ QueryError | v)) m ⇒ (e → QueryError) → Either e a → m a
queryError e = either throwQueryError pure ∘ lmap e
