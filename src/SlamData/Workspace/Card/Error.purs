{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Error
       ( module SlamData.Workspace.Card.Error
       , module CCE
       ) where

import SlamData.Prelude

import Quasar.Advanced.QuasarAF as QA
import Quasar.Error (QError, printQError)
import SlamData.GlobalError as GE

import SlamData.Workspace.Card.Cache.Error as CCE

data CardError
  = QuasarError QError
  | StringlyTypedError String
  | CacheCardError CCE.CacheError

quasarToCardError ∷ QError → CardError
quasarToCardError = QuasarError

cardToGlobalError ∷ CardError → Either String GE.GlobalError
cardToGlobalError = case _ of
  QuasarError qError → case qError of
    QA.PaymentRequired → Right GE.PaymentRequired
    QA.Unauthorized unauthDetails → Right (GE.Unauthorized unauthDetails)
    QA.Forbidden → Right GE.Forbidden
    err → Left (printQError err)
  StringlyTypedError err → Left err
  CacheCardError cce → Left (CCE.cacheErrorMessage cce)

throw ∷ ∀ m a. MonadThrow CardError m ⇒ Warn "You really don't want to" ⇒ String → m a
throw = throwError ∘ StringlyTypedError

throwCacheError ∷ ∀ m a. MonadThrow CardError m ⇒ CCE.CacheError → m a
throwCacheError = throwError ∘ CacheCardError

liftQ ∷ ∀ m a. MonadThrow CardError m ⇒ m (Either QError a) → m a
liftQ = flip bind (either (throwError ∘ quasarToCardError) pure)
