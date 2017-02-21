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


module SlamData.GlobalError where

import SlamData.Prelude

import Halogen.Query (HalogenM)

import Quasar.Error as QE

import SlamData.Notification (NotificationOptions)
import SlamData.Notification as N

data GlobalError
  = PaymentRequired
  | Unauthorized (Maybe QE.UnauthorizedDetails)
  | Forbidden

class GlobalErrorDSL m where
  raiseGlobalError ∷ GlobalError → m Unit

instance globalErrorDSLMaybeT ∷ (Monad m, GlobalErrorDSL m) ⇒ GlobalErrorDSL (MaybeT m) where
  raiseGlobalError = lift ∘ raiseGlobalError

instance globalErrorDSLExceptT ∷ (Monad m, GlobalErrorDSL m) ⇒ GlobalErrorDSL (ExceptT e m) where
  raiseGlobalError = lift ∘ raiseGlobalError

instance globalErrorDSLHalogenM ∷ (Monad m, GlobalErrorDSL m) ⇒ GlobalErrorDSL (HalogenM s f g p o m) where
  raiseGlobalError = lift ∘ raiseGlobalError

fromQError ∷ QE.QError → Either String GlobalError
fromQError = case _ of
  QE.PaymentRequired → Right PaymentRequired
  QE.Unauthorized unauthDetails → Right $ Unauthorized unauthDetails
  QE.Forbidden → Right Forbidden
  err → Left (QE.printQError err)

toQError ∷ GlobalError → QE.QError
toQError = case _ of
  PaymentRequired → QE.PaymentRequired
  Unauthorized unauthDetails → QE.Unauthorized unauthDetails
  Forbidden → QE.Forbidden

print ∷ GlobalError → String
print = case _ of
  PaymentRequired →
    "Payment is required to perform the current action."
  Unauthorized _ →
    "Please sign in to continue."
  Forbidden →
    "Your authorization credentials do not grant access to this resource."

toNotificationOptions ∷ GlobalError → NotificationOptions
toNotificationOptions =
  case _ of
    PaymentRequired →
      { notification: N.Error (print PaymentRequired)
      , detail: Nothing
      , timeout: Nothing
      , actionOptions: Nothing
      }
    Unauthorized unauthDetails →
      { notification: N.Error "You are not currently authorized"
      , detail: (\(QE.UnauthorizedDetails s) → N.Details s) <$> unauthDetails
      , timeout: Nothing
      , actionOptions:
          Just $ N.ActionOptions
            { message: "File listings and workspaces may appear empty or incomplete. Please sign in to continue."
            , actionMessage: "Sign in"
            , action: N.ExpandGlobalMenu
            }
      }
    Forbidden →
      { notification: N.Error (print Forbidden)
      , detail: Nothing
      , timeout: Nothing
      , actionOptions:
          Just $ N.ActionOptions
            { message: "File listings and workspaces may appear empty or incomplete. Please sign in as a user with permission to access this resource or contact your administrator."
            , actionMessage: "Sign in"
            , action: N.ExpandGlobalMenu
            }
      }
