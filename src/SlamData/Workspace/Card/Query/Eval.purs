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

module SlamData.Workspace.Card.Query.Eval
  ( evalQuery
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Path.Pathy as Path

import Quasar.Types (SQL)

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.Common (validateResources)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

evalQuery
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ Port.VarMap
  → SQL
  → m Port.TaggedResourcePort
evalQuery varMap sql = do
  resource ← CEM.temporaryOutputResource
  let
    varMap' = Port.renderVarMapValue <$> varMap
    backendPath =
      Left $ fromMaybe Path.rootDir (Path.parentDir resource)
  { inputs } ←
    CEM.liftQ $ lmap (QE.prefixMessage "Error compiling query") <$>
      QQ.compile backendPath sql varMap'
  validateResources inputs
  CEM.addSources inputs
  CEM.liftQ do
    QQ.viewQuery backendPath resource sql varMap'
    QFS.messageIfFileNotFound resource "Requested collection doesn't exist"
  pure { resource, tag: pure sql, varMap: Just varMap }
