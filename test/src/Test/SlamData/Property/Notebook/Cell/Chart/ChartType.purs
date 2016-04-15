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

module Test.SlamData.Property.Notebook.Card.Chart.ChartType where

import Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (Either(..))
import Data.List (toList)

import SlamData.Notebook.Card.Chart.ChartType (ChartType(..))

import Test.StrongCheck (QC, Result(..), class Arbitrary, quickCheck, (<?>))
import Test.StrongCheck.Gen (elements)

newtype ArbChartType = ArbChartType ChartType

runArbChartType :: ArbChartType -> ChartType
runArbChartType (ArbChartType m) = m

instance arbitraryArbChartType :: Arbitrary ArbChartType where
  arbitrary =
    ArbChartType <$>
      elements
        Pie
        (toList
          [ Line
          , Bar
          ])

check :: QC Unit
check = quickCheck $ runArbChartType >>> \ct ->
  case decodeJson (encodeJson ct) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right ct' -> ct == ct' <?> "ChartType failed to decode as encoded value"
