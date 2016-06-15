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

import Control.Monad.Eff as Eff
import Control.Monad.Aff.Free (class Affable, fromEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as EC

import Data.Lens ((^?))
import Data.Path.Pathy as Path
import Data.StrMap as SM

import Quasar.Types (SQL, FilePath)

import SlamData.FileSystem.Resource as R
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.API.Model as API
import SlamData.Workspace.Card.DownloadOptions.Component.State as DO
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Markdown.Component.State.Core as MDS
import SlamData.Workspace.Card.Markdown.Eval as MDE
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Save.Eval as Save
import SlamData.Workspace.Card.Search.Interpret as Search
import SlamData.Workspace.Card.Viz.Eval as VizE
import SlamData.Workspace.Card.Viz.Model as Viz
import SlamData.Workspace.FormBuilder.Item.Model as FBI

import Text.SlamSearch as SS
import Text.Markdown.SlamDown.Halogen.Component.State as SDH

data Eval
  = Pass
  | Query SQL
  | Search String
  | Save (Maybe String)
  | Error String
  | Markdown String
  | MarkdownForm MD.Model
  | OpenResource R.Resource
  | API API.Model
  | Viz Viz.Model
  | DownloadOptions DO.State
  | Draftboard

instance showEval ∷ Show Eval where
  show =
    case _ of
      Pass → "Pass"
      Query str → "Query " <> show str
      Search str → "Search " <> show str
      Save str → "Save " <> show str
      Error str → "Error " <> show str
      Markdown str → "Markdown " <> show str
      OpenResource res → "OpenResource " <> show res
      MarkdownForm m → "MarkdownForm"
      Viz m → "Viz"
      API m → "API" -- TODO: I don't have time to write these show instances -js
      DownloadOptions m → "DownloadOptions"
      Draftboard → "Draftboard"

evalCard
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → Eval
  → CET.CardEvalT m Port.Port
evalCard input =
  case _, input.input of
    Error msg, _ →
      pure $ Port.CardError msg
    _, Just Port.Blocked →
      pure Port.Blocked
    Pass, Nothing →
      EC.throwError "Card expected an input value"
    Pass, Just port →
      pure port
    Draftboard, _ →
      pure Port.Draftboard
    Query sql, Just (Port.VarMap varMap) →
      Port.TaggedResource <$> evalQuery input sql varMap
    Query sql, _ →
      Port.TaggedResource <$> evalQuery input sql Port.emptyVarMap
    Markdown txt, _ →
      MDE.markdownEval input txt
    MarkdownForm model, _ →
      lift $ Port.VarMap <$> evalMarkdownForm input model
    Search query, Just (Port.TaggedResource { resource }) →
      Port.TaggedResource <$> evalSearch input query resource
    Save pathString, Just (Port.TaggedResource { resource }) →
      Port.TaggedResource <$> Save.eval input pathString resource
    OpenResource res, _ →
      Port.TaggedResource <$> evalOpenResource input res
    Viz model, _ →
      Port.ChartOptions <$> VizE.eval input model
    API model, _ →
      pure $ Port.VarMap $ evalAPI input model
    DownloadOptions { compress, options }, Just (Port.TaggedResource { resource }) →
      pure $ Port.DownloadOptions { resource, compress, options }
    e, i →
      EC.throwError $ "Card received unexpected input type; " <> show e <> " | " <> show i

evalAPI
  ∷ CET.CardEvalInput
  → API.Model
  → Port.VarMap
evalAPI info model =
  foldl alg SM.empty model.items
  where
    alg =
      flip \{ name, fieldType, defaultValue } ->
        maybe id (SM.insert name) $
          SM.lookup name info.globalVarMap
            <|> (FBI.defaultValueToVarMapValue fieldType =<< defaultValue)

evalMarkdownForm
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → MD.Model
  → m Port.VarMap
evalMarkdownForm info model = do
  let desc = SDH.formDescFromDocument model.input
  -- TODO: find a way to smash these annotations if possible -js
  fromEff (MDS.formStateToVarMap desc model.state ∷ Eff.Eff SlamDataEffects Port.VarMap)

evalOpenResource
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → R.Resource
  → CET.CardEvalT m Port.TaggedResourcePort
evalOpenResource info res = do
   filePath ← maybe (EC.throwError "No resource is selected") pure $ res ^? R._filePath
   msg ←
     QFS.messageIfFileNotFound
       filePath
       ("File " ⊕ Path.printPath filePath ⊕ " doesn't exist")
     # lift
   case msg of
     Right Nothing → pure { resource: filePath, tag: Nothing }
     Right (Just err) →
       EC.throwError err
     Left exn →
       EC.throwError $ Exn.message exn

evalQuery
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → SQL
  → Port.VarMap
  → CET.CardEvalT m Port.TaggedResourcePort
evalQuery info sql varMap = do
  let
    varMap' = Port.renderVarMapValue <$> varMap
    resource = CET.temporaryOutputResource info
    backendPath = Left $ fromMaybe Path.rootDir (Path.parentDir resource)
  plan ← lift $ QQ.compile backendPath sql varMap'
  case plan of
    Left err → EC.throwError $ "Error compiling query: " ⊕ Exn.message err
    Right p → pure unit -- WC.tell ["Plan: " ⊕ p]
  liftQ do
    QQ.viewQuery backendPath resource sql varMap'
    QFS.messageIfFileNotFound resource "Requested collection doesn't exist"
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

  let
    template = Search.queryToSQL fields query
    sql = QQ.templated resource template
    outputResource = CET.temporaryOutputResource info

  -- WC.tell ["Generated SQL: " ⊕ sql]

  plan ← lift $ QQ.compile (Right resource) sql SM.empty

  case plan of
    Left err → EC.throwError $ "Error compiling query: " ⊕ Exn.message err
    Right p → pure unit -- WC.tell ["Plan: " ⊕ p]

  liftQ do
    QQ.viewQuery (Right resource) outputResource template SM.empty
    QFS.messageIfFileNotFound
      outputResource
      "Error making search temporary resource"

  pure { resource: outputResource, tag: pure sql }

liftQ ∷ ∀ m a. Monad m ⇒ m (Either Exn.Error a) → CET.CardEvalT m a
liftQ = either (EC.throwError ∘ Exn.message) pure <=< lift

runEvalCard
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → Eval
  → m Port.Port
runEvalCard input =
  CET.runCardEvalT ∘
    evalCard input
