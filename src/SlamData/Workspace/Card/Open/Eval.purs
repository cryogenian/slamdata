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

module SlamData.Workspace.Card.Open.Eval
  ( evalOpen
  ) where

import SlamData.Prelude

import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Lens ((^?), (?~))
import Data.Path.Pathy as Path

import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Open.Model as Open
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM

import SqlSquared as Sql

import Utils.Path (FilePath)
import Utils.SqlSquared (all)

evalOpen
  ∷ ∀ m
  . ( MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , MonadAsk CEM.CardEnv m
    , QuasarDSL m
    )
  ⇒ Open.Model
  → Port.DataMap
  → m Port.Out
evalOpen model varMap = case model of
  Nothing → noResource
  Just (Open.Resource res) → do
    filePath ← maybe noResource pure $ res ^? R._filePath
    checkPath filePath >>= case _ of
      Nothing → do
        CEM.addSource filePath
        pure (Port.resourceOut (Port.Path filePath))
      Just err → CEM.throw err
  Just (Open.Variable (VM.Var var)) → do
    res ← CEM.temporaryOutputResource
    let
      sql =
        Sql.buildSelect
          $ all
          ∘ (Sql._relations ?~ Sql.VariRelation { vari: var, alias: Nothing })
      varMap' =
        map (Sql.print ∘ unwrap) $ Port.flattenResources varMap
      backendPath =
        fromMaybe Path.rootDir $ Path.parentDir res

    CEM.liftQ $ QQ.viewQuery res sql varMap'
    pure $ Port.resourceOut $ Port.View res (Sql.print $ sql) varMap

  where
  noResource ∷ ∀ a. m a
  noResource =
    CEM.throw "No resource is selected"

  checkPath ∷ FilePath → m (Maybe String)
  checkPath filePath =
    CEM.liftQ $ QFS.messageIfFileNotFound filePath $
      "File " ⊕ Path.printPath filePath ⊕ " doesn't exist"
