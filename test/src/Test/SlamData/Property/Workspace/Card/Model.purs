{-
Copyright 2015 SlamData, Inc.

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

module Test.SlamData.Property.Workspace.Card.Model
  ( check
  , checkCardEquality
  ) where

import SlamData.Prelude

import Data.Argonaut as J

import SlamData.Workspace.Card.Model as Card

import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC

check ∷ forall eff. SC.SC eff Unit
check =
  -- We have _a lot of_ model types
  SC.quickCheck' 1000 \model →
    case Card.decode (Card.encode model) of
      Left err → SC.Failed $ "Decode failed: " <> err
      Right model' → checkCardEquality model model'

checkCardEquality ∷ Card.AnyCardModel → Card.AnyCardModel → SC.Result
checkCardEquality model model' =
  model ≡ model'
    <?> ("model mismatch:\n "
         <> show (J.encodeJson model)
         <> "\n" <> show (J.encodeJson model'))
