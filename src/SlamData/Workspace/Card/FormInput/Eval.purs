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

module SlamData.Workspace.Card.FormInput.Eval
  ( evalTextLike
  , evalLabeled
  , module SlamData.Workspace.Card.FormInput.Model
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Foldable as F
import Data.Lens ((^.))
import Data.Path.Pathy as Path
import Data.StrMap as SM

import Quasar.Types (SQL)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))
import SlamData.Workspace.Card.Eval.Common (validateResources, escapeCursor)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.FormInput.Model (Model(..))
import SlamData.Workspace.Card.FormInput.LabeledRenderer.Model as LR
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model as TLR
import SlamData.Workspace.Card.Setups.Semantics as Sem

eval
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ SQL
  → String
  → Port.VarMapValue
  → Port.Resource
  → m Port.Out
eval sql var val r = do
  resource ← CEM.temporaryOutputResource
  let
    backendPath =
      Left $ fromMaybe Path.rootDir (Path.parentDir (r ^. Port._filePath))

  { inputs } ←
    CEM.liftQ $ lmap (QE.prefixMessage "Error compiling query") <$>
      QQ.compile backendPath sql SM.empty

  validateResources inputs
  CEM.addSources inputs
  CEM.liftQ do
    QQ.viewQuery backendPath resource sql SM.empty
    QFS.messageIfFileNotFound resource "Requested collection doesn't exist"
  let
    var' =
      if var ≡ Port.defaultResourceVar
         then var <> "2"
         else var
    varMap =
      SM.fromFoldable
        [ Port.defaultResourceVar × Left (Port.View resource sql SM.empty)
        , var' × Right val
        ]
  pure (Port.ResourceKey Port.defaultResourceVar × varMap)

evalLabeled
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ LR.Model
  → Port.SetupLabeledFormInputPort
  → Port.Resource
  → m Port.Out
evalLabeled m p r =
  eval sql p.name (Port.QueryExpr prettySelection) r
  where
    semantics =
      foldMap Sem.semanticsToSQLStrings m.selected

    selection =
      "(" <> (F.intercalate "," semantics) <> ")"

    prettySelection =
      case semantics of
        [ a ] → a
        _     → selection

    sql =
      "SELECT * FROM `"
      <> Path.printPath (r ^. Port._filePath)
      <> "` AS res"
      <> " WHERE res"
      <> (escapeCursor p.cursor)
      <> " IN "
      <> selection

evalTextLike
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ TLR.Model
  → Port.SetupTextLikeFormInputPort
  → Port.Resource
  → m Port.Out
evalTextLike m p r =
  eval sql p.name (Port.QueryExpr selection) r
  where
  selection =
    case p.formInputType of
      Text → show m.value
      _ → m.value

  sql =
    "SELECT * FROM `"
    <> Path.printPath (r ^. Port._filePath)
    <> "` AS res"
    <> " WHERE res"
    <> (escapeCursor p.cursor)
    <> " = "
    <> selection
