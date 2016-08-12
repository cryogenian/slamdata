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

module SlamData.Quasar.Error where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class (class MonadError, throwError)

import Quasar.Error (QError(..))

throw ∷ ∀ m a. (MonadError QError m) ⇒ String → m a
throw = throwError ∘ msgToQError

msgToQError :: String → QError
msgToQError = Error ∘ Exn.error
