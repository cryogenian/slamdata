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

module SlamData.Workspace.Card.Chart.ChartType
  ( ChartType(..)
  , isPie
  , isLine
  , isBar
  , parseChartType
  , printChartType
  ) where

import SlamData.Prelude
import Data.Argonaut (fromString, class EncodeJson, class DecodeJson, decodeJson)
import Data.List as L
import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

data ChartType = Pie | Line | Bar

isPie :: ChartType -> Boolean
isPie Pie = true
isPie _ = false

isLine :: ChartType -> Boolean
isLine Line = true
isLine _ = false

isBar :: ChartType -> Boolean
isBar Bar = true
isBar _ = false

parseChartType :: String -> Either String ChartType
parseChartType "pie" = pure Pie
parseChartType "line" = pure Line
parseChartType "bar" = pure Bar
parseChartType _ = Left "incorrect chartType"

printChartType :: ChartType -> String
printChartType Pie = "pie"
printChartType Line = "line"
printChartType Bar = "bar"

derive instance genericChartType :: Generic ChartType
derive instance eqChartType :: Eq ChartType
derive instance ordChartType :: Ord ChartType

instance encodeJsonChartType :: EncodeJson ChartType where
  encodeJson = fromString <<< printChartType

instance decodeJsonChartType :: DecodeJson ChartType where
  decodeJson json = decodeJson json >>= parseChartType

instance arbitraryChartType ∷ SC.Arbitrary ChartType where
  arbitrary = Gen.elements Pie $ L.toList [ Pie, Line, Bar ]
