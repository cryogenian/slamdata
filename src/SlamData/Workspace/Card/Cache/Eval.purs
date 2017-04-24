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

module SlamData.Workspace.Card.Cache.Eval where

import SlamData.Prelude

import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Lens ((^.), (.~))
import Data.Path.Pathy as Path
import Data.StrMap as SM

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

import SqlSquare as Sql

import Utils.Path as PU
import Utils.SqlSquare (tableRelation, all)

eval
  ∷ ∀ m
  . MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CEM.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ Maybe String
  → Port.Resource
  → m Port.Out
eval mfp resource =
  Port.resourceOut <$> case mfp of
    Nothing → do
      tmp ← CEM.temporaryOutputResource
      eval' tmp resource
    Just pt →
      case PU.parseAnyPath pt of
        Just (Right fp) → eval' fp resource
        _ → CEM.throw $ pt ⊕ " is not a valid file path"

eval'
  ∷ ∀ m
  . MonadThrow CEM.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ FilePath
  → Port.Resource
  → m Port.Resource
eval' tmp resource = do
  let
    filePath = resource ^. Port._filePath
    backendPath = fromMaybe Path.rootDir $ Path.parentDir filePath
    sql =
      Sql.buildSelect
        $ all
        ∘ (Sql._relations .~ tableRelation filePath)
  outputResource ← CEM.liftQ $
    QQ.fileQuery backendPath tmp sql SM.empty
  CEM.liftQ $ QFS.messageIfFileNotFound
    outputResource
    "Error saving file, please try another location"
  -- TODO: this error message is pretty obscure. I think it occurs when a query
  -- is like "SELECT * FROM t" and quasar does no work. I'm not sure what the
  -- behaviour of Save should be in that case - perhaps instead of failing it
  -- could create a view so that a resource will actually be created. Debateable
  -- as to whether that is "right", but at least it means a resource will exist
  -- in the expected location, and the rest of the deck can run as the Save
  -- failing has not effect on the output. -gb
  when (tmp /= outputResource)
    $ CEM.throw
    $ "Resource: " ⊕ Path.printPath outputResource ⊕ " hasn't been modified"
  CEM.addSource filePath
  CEM.addCache outputResource
  pure (Port.Path outputResource)
