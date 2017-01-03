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

module SlamData.Notification
  ( Notification(..)
  , NotificationOptions
  , ActionOptions(..)
  , Action(..)
  , Details(..)
  , class NotifyDSL
  , notify
  , optionsWithoutActionEq
  , info
  , warn
  , error
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Free (Free, liftF)

import Data.Time.Duration (Milliseconds)

import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenF as HF

data Notification
  = Info String
  | Warning String
  | Error String

derive instance genericNotification :: Generic Notification

derive instance eqNotification ∷ Eq Notification

data Action
  = ExpandGlobalMenu
  | Fulfill (AVar Unit)

type NotificationOptions =
  { notification ∷ Notification
  , detail ∷ Maybe Details
  , actionOptions ∷ Maybe ActionOptions
  , timeout ∷ Maybe Milliseconds
  }

optionsWithoutActionEq
  ∷ NotificationOptions
  → NotificationOptions
  → Boolean
optionsWithoutActionEq x y =
  isNothing x.actionOptions
    && isNothing y.actionOptions
    && x.notification == y.notification
    && x.timeout == y.timeout
    && y.detail == y.detail

newtype Details = Details String

derive instance eqDetails ∷ Eq Details

newtype ActionOptions =
  ActionOptions
    { messagePrefix ∷ String
    , actionMessage ∷ String
    , messageSuffix ∷ String
    , action ∷ Action
    }

class NotifyDSL m where
  notify ∷ Notification → Maybe Details → Maybe Milliseconds → Maybe ActionOptions → m Unit

instance notifyDSLFree ∷ NotifyDSL m ⇒ NotifyDSL (Free m) where
  notify n d m a = liftF $ notify n d m a

instance notifyDSLMaybeT ∷ (Monad m, NotifyDSL m) ⇒ NotifyDSL (MaybeT m) where
  notify n d m a = lift $ notify n d m a

instance notifyDSLExceptT ∷ (Monad m, NotifyDSL m) ⇒ NotifyDSL (ExceptT e m) where
  notify n d m a = lift $ notify n d m a

instance notifyDSLHFC ∷ NotifyDSL g ⇒ NotifyDSL (HF.HalogenFP ES.EventSource s f g) where
  notify n d m a = HF.QueryHF $ notify n d m a

instance notifyDSLHFP ∷ NotifyDSL g ⇒ NotifyDSL (HF.HalogenFP ES.ParentEventSource s f (Free (HF.HalogenFP ES.EventSource s' f' g))) where
  notify n d m a = HF.QueryHF $ notify n d m a

info
  ∷ ∀ m
  . NotifyDSL m
  ⇒ String
  → Maybe Details
  → Maybe Milliseconds
  → Maybe ActionOptions
  → m Unit
info = notify <<< Info

warn
  ∷ ∀ m
  . NotifyDSL m
  ⇒ String
  → Maybe Details
  → Maybe Milliseconds
  → Maybe ActionOptions
  → m Unit
warn = notify <<< Warning

error
  ∷ ∀ m
  . NotifyDSL m
  ⇒ String
  → Maybe Details
  → Maybe Milliseconds
  → Maybe ActionOptions
  → m Unit
error = notify <<< Error
