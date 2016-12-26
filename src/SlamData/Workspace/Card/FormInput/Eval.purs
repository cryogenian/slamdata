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

import Data.Argonaut as J
import Data.Foldable as F
import Data.Path.Pathy as Path
import Data.String as Str
import Data.StrMap as SM

import Quasar.Types (SQL)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))
import SlamData.Workspace.Card.Eval.Common (validateResources)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.FormInput.Model (Model(..))
import SlamData.Workspace.Card.FormInput.LabeledRenderer.Model as LR
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model as TLR
import SlamData.Workspace.Card.BuildChart.Semantics as Sem

type TaggedResourceAndCursor r =
  { taggedResource ∷ Port.TaggedResourcePort, cursor ∷ J.JCursor | r}

eval
  ∷ ∀ m a r
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ (a → TaggedResourceAndCursor r → SQL)
  → a
  → TaggedResourceAndCursor r
  → m Port.Port
eval f m p = do
  resource ← CEM.temporaryOutputResource
  let
    backendPath =
      Left $ fromMaybe Path.rootDir (Path.parentDir resource)
    sql = f m p

  { inputs } ←
    CEM.liftQ $ lmap (QE.prefixMessage "Error compiling query") <$>
      QQ.compile backendPath sql SM.empty

  validateResources inputs
  CEM.addSources inputs
  CEM.liftQ do
    QQ.viewQuery backendPath resource sql SM.empty
    QFS.messageIfFileNotFound resource "Requested collection doesn't exist"
  pure $ Port.TaggedResource { resource, tag: pure sql, varMap: Nothing }

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
  → m Port.Port
evalLabeled = eval \m p →
  "select * from `"
  <> Path.printPath p.taggedResource.resource
  <> "`"
  <> " where "
  <> (Str.drop 1 $ show p.cursor)
  <> " in ("
  <> (F.intercalate "," $ foldMap Sem.semanticsToSQLStrings m.selected)
  <> ")"

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
  → m Port.Port
evalTextLike = eval \m p →
  "select * from `"
  <> Path.printPath p.taggedResource.resource
  <> "`"
  <> " where "
  <> (Str.drop 1 $ show p.cursor)
  <> " = "
  <> (case p.formInputType of
         Text → "\"" <> m.value <> "\""
         _ → m.value)
