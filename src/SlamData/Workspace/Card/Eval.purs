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

module SlamData.Workspace.Card.Eval where

import SlamData.Prelude

import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as EC
import Control.Monad.Writer.Class as WC

import Data.Path.Pathy as Path
import Data.StrMap as SM

import Quasar.Types (SQL, FilePath)

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Search.Interpret as Search

import Text.SlamSearch as SS
import Utils.Path as PathUtils

data Eval
  = Pass
  | Query SQL
  | Search String
  | Save String
  | Error String

evalCard
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → Eval
  → CET.CardEvalT m Port.Port
evalCard input = case _, input.input of
  Error msg, _ →
    pure $ Port.CardError msg
  _, Just Port.Blocked →
    pure Port.Blocked
  Pass, Nothing →
    EC.throwError "Card expected an input value"
  Pass, Just port →
    pure port
  Query sql, Just (Port.VarMap varMap) →
    Port.TaggedResource <$> evalQuery input sql varMap
  Query sql, Just (Port.TaggedResource _) →
    Port.TaggedResource <$> evalQuery input sql Port.emptyVarMap
  Search query, Just (Port.TaggedResource { resource }) →
    Port.TaggedResource <$> evalSearch input query resource
  Save pathString, Just (Port.TaggedResource { resource }) →
    Port.TaggedResource <$> evalSave input pathString resource
  _, _ →
    EC.throwError "Card received unexpected input type"

evalQuery
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → SQL
  → Port.VarMap
  → CET.CardEvalT m Port.TaggedResourcePort
evalQuery info sql varMap = do
  let varMap' = Port.renderVarMapValue <$> varMap
  let resource = CET.temporaryOutputResource info
  let backendPath = Left $ fromMaybe Path.rootDir (Path.parentDir resource)
  plan ← lift $ QQ.compile backendPath sql varMap'
  liftQ do
    QQ.viewQuery backendPath resource sql varMap'
    QFS.messageIfFileNotFound resource "Requested collection doesn't exist"
  for_ plan \p → WC.tell ["Plan: " ⊕ p]
  pure { resource, tag: pure sql }

evalSearch
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → String
  → FilePath
  → CET.CardEvalT m Port.TaggedResourcePort
evalSearch info queryText resource = do
  query ← case SS.mkQuery queryText of
    Left _ → EC.throwError "Incorrect query string"
    Right q → pure q

  fields ← liftQ do
    QFS.messageIfFileNotFound
      resource
      ("Input resource " ⊕ Path.printPath resource ⊕ " doesn't exist")
    QQ.fields resource

  let template = Search.queryToSQL fields query
  let sql = QQ.templated resource template
  let outputResource = CET.temporaryOutputResource info

  WC.tell ["Generated SQL: " ⊕ sql]

  plan ← lift $ QQ.compile (Right resource) sql SM.empty

  case plan of
    Left err → EC.throwError $ "Error compiling query: " ⊕ Exn.message err
    Right p → WC.tell ["Plan: " ⊕ p]

  liftQ do
    QQ.viewQuery (Right resource) outputResource template SM.empty
    QFS.messageIfFileNotFound
      outputResource
      "Error making search temporary resource"

  pure { resource: outputResource, tag: pure sql }

evalSave
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → String
  → FilePath
  → CET.CardEvalT m Port.TaggedResourcePort
evalSave info pt resource =
  case PathUtils.parseAnyPath pt of
    Just (Right fp) → do

      outputResource ← liftQ $
        QQ.fileQuery resource fp "select * from {{path}}" SM.empty

      liftQ $ QFS.messageIfFileNotFound
        outputResource
        "Error saving file, please try another location"

      when (fp /= outputResource)
        $ EC.throwError
        $ "Resource: " ⊕ Path.printPath outputResource ⊕ " hasn't been modified"

      WC.tell ["Resource successfully saved as: " ⊕ Path.printPath fp]

      pure { resource: outputResource, tag: Nothing }
    _ →
      EC.throwError $ pt ⊕ " is incorrect file path"

liftQ ∷ ∀ m a. Monad m ⇒ m (Either Exn.Error a) → CET.CardEvalT m a
liftQ = either (EC.throwError ∘ Exn.message) pure <=< lift

runEvalCard
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → Eval
  → m CET.CardEvalResult
runEvalCard input port = CET.runCardEvalT $ evalCard input port
