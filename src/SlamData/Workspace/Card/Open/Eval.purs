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

import Data.Lens ((^?))
import Data.Path.Pathy as Path

import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

evalOpen
  ∷ ∀ m
  . ( MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , QuasarDSL m
    )
  ⇒ R.Resource
  → m Port.TaggedResourcePort
evalOpen res = do
  filePath ←
    maybe (CEM.throw "No resource is selected") pure
      $ res ^? R._filePath
  msg ←
    CEM.liftQ $ QFS.messageIfFileNotFound filePath $
      "File " ⊕ Path.printPath filePath ⊕ " doesn't exist"
  case msg of
    Nothing → do
      CEM.addSource filePath
      pure { resource: filePath, tag: Nothing, varMap: Nothing }
    Just err →
      CEM.throw err
