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

module SlamData.Quasar.Error
  ( module SlamData.Quasar.Error
  , module Quasar.Error
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exn

import Quasar.Error (QError(..), UnauthorizedDetails(..), printQError)

throw ∷ ∀ m a. (MonadError QError m) ⇒ String → m a
throw = throwError ∘ msgToQError

msgToQError :: String → QError
msgToQError = Error ∘ Exn.error

-- | Prefixes the `Error` case of a `QError` with an additional message. This
-- | has no effect on the other `QError` constructors.
prefixMessage :: String → QError → QError
prefixMessage msg = case _ of
  Error err -> Error $ Exn.error (msg <> ": " <> Exn.message err)
  qe -> qe

isUnauthorized ∷ QError → Boolean
isUnauthorized =
  case _ of
    Unauthorized _ → true
    _ → false
