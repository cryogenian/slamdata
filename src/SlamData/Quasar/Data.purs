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

module SlamData.Quasar.Data
  ( makeFile
  , save
  , load
  , delete
  ) where

import SlamData.Prelude

import Data.Argonaut as JS

import Quasar.Advanced.QuasarAF as QF
import Quasar.Data (QData(..), JSONMode(..))
import Quasar.Error (QError)
import Quasar.Types (FilePath, AnyPath)

import SlamData.Quasar.Error (throw)
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)

makeFile
  ∷ ∀ m
  . QuasarDSL m
  ⇒ FilePath
  → QData
  → m (Either QError Unit)
makeFile path = liftQuasar ∘ QF.writeFile path

-- | Saves a single JSON value to a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
save
  ∷ ∀ m
  . QuasarDSL m
  ⇒ FilePath
  → JS.Json
  → m (Either QError Unit)
save path json = liftQuasar $ QF.writeFile path (JSON Readable [json])

-- | Loads a single JSON value from a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
load
  ∷ ∀ m
  . (Functor m, QuasarDSL m)
  ⇒ FilePath
  → m (Either QError JS.Json)
load file =
  liftQuasar (QF.readFile Readable file Nothing) <#> case _ of
    Right [file] → Right file
    Right _ → throw "Unexpected result when loading value from file"
    Left err → Left err

delete
  ∷ ∀ m
  . (Functor m, QuasarDSL m)
  ⇒ AnyPath
  → m (Either QError Unit)
delete = liftQuasar ∘ QF.deleteData
