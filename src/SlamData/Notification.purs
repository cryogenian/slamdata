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
  , NotificationAction(..)
  , Detail(..)
  , class NotifyDSL
  , notify
  , optionsWithSimpleDetailsEq
  , info
  , warn
  , error
  ) where

import SlamData.Prelude

import Data.Generic (gEq)

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

instance eqNotification ∷ Eq Notification where
  eq = gEq

data NotificationAction
  = ExpandGlobalMenu
  | Fulfill (AVar Unit)

type NotificationOptions =
  { notification ∷ Notification
  , detail ∷ Maybe Detail
  , timeout ∷ Maybe Milliseconds
  }

optionsWithSimpleDetailsEq
  ∷ NotificationOptions
  → NotificationOptions
  → Boolean
optionsWithSimpleDetailsEq x y =
  x.notification == y.notification && x.timeout == y.timeout && detailsAreEq
  where
  detailsAreEq =
    case x.detail, y.detail of
      Nothing, Nothing → true
      Just xDetail, Just yDetail → xDetail `simpleDetailEq` yDetail
      _, _ → false

data Detail
  = SimpleDetail String
  | ActionDetail
      { messagePrefix ∷ String
      , actionMessage ∷ String
      , messageSuffix ∷ String
      , action ∷ NotificationAction
      }

simpleDetailEq ∷ Detail → Detail → Boolean
simpleDetailEq =
  case _, _ of
    SimpleDetail x, SimpleDetail y → x == y
    _, _ → false

class NotifyDSL m where
  notify ∷ Notification → Maybe Detail → Maybe Milliseconds → m Unit

instance notifyDSLFree ∷ NotifyDSL m ⇒ NotifyDSL (Free m) where
  notify n d m = liftF $ notify n d m

instance notifyDSLMaybeT ∷ (Monad m, NotifyDSL m) ⇒ NotifyDSL (MaybeT m) where
  notify n d m = lift $ notify n d m

instance notifyDSLExceptT ∷ (Monad m, NotifyDSL m) ⇒ NotifyDSL (ExceptT e m) where
  notify n d m = lift $ notify n d m

instance notifyDSLHFC ∷ NotifyDSL g ⇒ NotifyDSL (HF.HalogenFP ES.EventSource s f g) where
  notify n d m = HF.QueryHF $ notify n d m

instance notifyDSLHFP ∷ NotifyDSL g ⇒ NotifyDSL (HF.HalogenFP ES.ParentEventSource s f (Free (HF.HalogenFP ES.EventSource s' f' g))) where
  notify n d m = HF.QueryHF $ notify n d m

info
  ∷ ∀ m
  . NotifyDSL m
  ⇒ String
  → Maybe Detail
  → Maybe Milliseconds
  → m Unit
info = notify <<< Info

warn
  ∷ ∀ m
  . NotifyDSL m
  ⇒ String
  → Maybe Detail
  → Maybe Milliseconds
  → m Unit
warn = notify <<< Warning

error
  ∷ ∀ m
  . NotifyDSL m
  ⇒ String
  → Maybe Detail
  → Maybe Milliseconds
  → m Unit
error = notify <<< Error
