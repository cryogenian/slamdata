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

module SlamData.Workspace.Model
  ( Workspace
  , emptyWorkspace
  , encode
  , decode
  , getRoot
  , setRoot
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)

import Quasar.Types (FilePath)

import SlamData.Quasar.Data as QD
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Deck.DeckId (DeckId)

type Workspace =
  { root ∷ Maybe DeckId
  }

emptyWorkspace ∷ Workspace
emptyWorkspace = { root: Nothing }

encode ∷ Workspace → Json
encode ws
   = "root" := ws.root
  ~> jsonEmptyObject

decode ∷ Json → Either String Workspace
decode = decodeJson >=> \obj ->
  { root: _
  } <$> obj .? "root"

getRoot
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ FilePath
  → m (Either QE.QError DeckId)
getRoot file = runExceptT do
  json ← ExceptT $ QD.load file
  ws ← ExceptT $ pure $ lmap QE.msgToQError $ decode json
  maybe (ExceptT $ pure $ Left (QE.msgToQError "No root")) pure ws.root

setRoot
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ FilePath
  → DeckId
  → m (Either QE.QError Unit)
setRoot file root =
  QD.save file $ encode { root: Just root }
