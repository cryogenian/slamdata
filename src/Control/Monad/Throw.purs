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

module Control.Monad.Throw where

import Prelude
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)

class Monad m ⇐ MonadThrow e m where
  throw ∷ ∀ a. e → m a

instance eitherMonadThrow ∷ MonadThrow e (Either e) where
  throw = Left

instance exceptTMonadThrow ∷ Monad m ⇒ MonadThrow e (ExceptT e m) where
  throw = ExceptT <<< pure <<< Left

note ∷ ∀ m e a. MonadThrow e m ⇒ e → Maybe a → m a
note err = maybe (throw err) pure

noteError ∷ ∀ m a. MonadThrow Exn.Error m ⇒ String → Maybe a → m a
noteError = note <<< Exn.error

