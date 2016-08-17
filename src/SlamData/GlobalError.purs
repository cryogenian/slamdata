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

import Quasar.Error as QE

data GlobalError
  = PaymentRequired
  | Unauthorized

fromQError ∷ QE.QError → Either String GlobalError
fromQError = case _ of
  QE.PaymentRequired → Right PaymentRequired
  QE.Unauthorized → Right Unauthorized
  err → Left (QE.printQError err)

toQError ∷ GlobalError → QE.QError
toQError = case _ of
  PaymentRequired → QE.PaymentRequired
  Unauthorized → QE.Unauthorized

print ∷ GlobalError → String
print = case _ of
  PaymentRequired →
    "Payment is required to perform the current action."
  Unauthorized →
    "Please sign in to continue."
