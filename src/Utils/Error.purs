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

module Utils.Error where

import Prelude

import Control.Monad.Except (Except, runExcept)
import Data.Either (Either, either)
import Data.Maybe (Maybe, maybe)
import Control.Monad.Error.Class (class MonadError, throwError)

fromMaybe ∷ forall a b m. (MonadError b m) ⇒ b → Maybe a → m a
fromMaybe x =
  maybe (throwError x) pure

fromEither ∷ forall a b m. (MonadError b m) ⇒ Either b a → m a
fromEither =
  either throwError pure

fromExcept ∷ forall a b m. (MonadError b m) ⇒ Except b a → m a
fromExcept =
  fromEither <<< runExcept

