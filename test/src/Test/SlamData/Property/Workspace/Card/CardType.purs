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

module Test.SlamData.Property.Workspace.Card.CardType where

import SlamData.Prelude

import Data.Argonaut (encodeJson, decodeJson)

import SlamData.Workspace.Card.CardType (CardType(..), AceMode(..))
import SlamData.Workspace.Card.CardType.ChartType (allChartTypes)

import Test.StrongCheck (SC, Result(..), quickCheck, (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen (allInArray)

newtype ArbCardType = ArbCardType CardType

runArbCardType ∷ ArbCardType → CardType
runArbCardType (ArbCardType m) = m

instance arbitraryArbCardType ∷ Arbitrary ArbCardType where
  arbitrary =
    allInArray
      $ map ArbCardType
      $ [ Ace MarkdownMode
        , Ace SQLMode
        , Search
        , Chart
        , Markdown
        , Table
        ]
      ⊕ map ChartOptions allChartTypes


check ∷ forall eff. SC eff Unit
check = quickCheck $ runArbCardType ⋙ \ct →
  case decodeJson (encodeJson ct) of
    Left err → Failed $ "Decode failed: " <> err
    Right ct' → ct == ct' <?> "CardType failed to decode as encoded value"
