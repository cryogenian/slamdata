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

module SlamData.Workspace.Card.Open.Eval (evalOpen) where

import SlamData.Prelude

import Control.Monad.Writer.Class (class MonadTell)
import Data.Functor.Mu (Mu)
import Data.Json.Extended as EJ
import Data.Lens ((^?), (?~), (.~))
import Data.List as L
import Data.Path.Pathy as Path
import Data.StrMap as SM
import Matryoshka as M
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Open.Error (OpenError(..), throwOpenError)
import SlamData.Workspace.Card.Open.Model as Open
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SqlSquared as Sql
import Utils.SqlSquared (all)

evalOpen
  ∷ ∀ m v
  . MonadThrow (Variant (open ∷ OpenError, qerror ∷ CE.QError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ Open.Model
  → Port.DataMap
  → m Port.Out
evalOpen model varMap = case model of
  Nothing → throwOpenError OpenNoResourceSelected
  Just (Open.Resource res) → do
    filePath ← maybe (throwOpenError OpenNoFileSelected) pure $ res ^? R._filePath
    checkPath filePath >>= case _ of
      Nothing → do
        CEM.addSource filePath
        pure (Port.resourceOut (Port.Path filePath))
      Just err → throwOpenError err
  Just (Open.Variable (VM.Var var)) → do
    res ← CEM.temporaryOutputResource
    let
      sql =
        case SM.lookup var varMap of
          Just (Right v) → do
            -- If the var is an ident, use `SELECT * FROM :ident`
            -- If the var is an literal, use `SELECT :literal AS literal`
            -- If the var is any other expr, use it directly
            case unwrapParens (unwrap v) of
              Sql.Ident _ →
                Sql.buildSelect
                  $ all
                  ∘ (Sql._relations ?~ Sql.VariRelation { vari: var, alias: Nothing })
              Sql.Literal _ →
                selectVarAsVar var v
              Sql.SetLiteral _ →
                selectVarAsVar var v
              _ →
                unwrap v
          _ →
            -- This case should be impossible - if we selected a var in the
            -- card, the var should exist during eval
            Sql.set L.Nil
      varMap' =
        map (Sql.print ∘ unwrap) $ Port.flattenResources varMap
      backendPath =
        fromMaybe Path.rootDir $ Path.parentDir res
    CE.liftQ $ QQ.viewQuery res sql varMap'
    pure $ Port.resourceOut $ Port.View res (Sql.print sql) varMap

  where

  selectVarAsVar ∷ String → VM.VarMapValue → Sql.Sql
  selectVarAsVar var v =
    Sql.buildSelect
      $ Sql._projections .~ pure (Sql.projection (unwrap v) # Sql.as var)

  unwrapParens ∷ Sql.Sql → Sql.SqlF EJ.EJsonF (Mu (Sql.SqlF EJ.EJsonF))
  unwrapParens expr =
    case M.project expr of
      Sql.Parens expr' → unwrapParens expr'
      expr' → expr'

  checkPath filePath =
    CE.liftQ $ QFS.messageIfFileNotFound filePath $ OpenFileNotFound (Path.printPath filePath)
