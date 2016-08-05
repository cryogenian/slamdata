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

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.Bus (Bus, Cap)
import Control.Monad.Aff.Free (class Affable)

import Data.Argonaut as JS

import Quasar.Advanced.QuasarAF as QF
import Quasar.Data (QData(..), JSONMode(..))
import Quasar.Error (lowerQError)
import Quasar.Types (FilePath, AnyPath)

import SlamData.Quasar.Aff (QEff, runQuasarF)
import SlamData.Quasar.Auth.Reauthentication (EIdToken)

makeFile
  ∷ ∀ eff r m
  . Affable (QEff eff) m
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → FilePath
  → QData
  → m (Either Exn.Error Unit)
makeFile requestNewIdTokenBus path content =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError <$>
    QF.writeFile path content

-- | Saves a single JSON value to a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
save
  ∷ ∀ eff r m
  . Affable (QEff eff) m
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → FilePath
  → JS.Json
  → m (Either Exn.Error Unit)
save requestNewIdTokenBus path json =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError <$>
    QF.writeFile path (JSON Readable [json])

-- | Loads a single JSON value from a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
load
  ∷ ∀ eff r m
  . (Functor m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → FilePath
  → m (Either String JS.Json)
load requestNewIdTokenBus file =
  runQuasarF requestNewIdTokenBus (QF.readFile Readable file Nothing) <#> case _ of
    Right [file] → Right file
    Right _ → Left "Unexpected result when loading value from file"
    Left err → Left (QF.printQError err)

delete
  ∷ ∀ eff r m
  . (Functor m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → AnyPath
  → m (Either Exn.Error Unit)
delete requestNewIdTokenBus path =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError <$>
    QF.deleteData path
