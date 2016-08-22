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
  , isArea
  , isScatter
  , isRadar
  , isFunnel
  , parseChartType
  , printChartType
  ) where

import SlamData.Prelude
import Data.Argonaut (fromString, class EncodeJson, class DecodeJson, decodeJson)
import Data.List as L
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data ChartType
  = Pie
  | Line
  | Bar
  | Area
  | Scatter
  | Radar
  | Funnel
  | Graph

isPie ∷ ChartType → Boolean
isPie Pie = true
isPie _ = false

isLine ∷ ChartType → Boolean
isLine Line = true
isLine _ = false

isBar ∷ ChartType → Boolean
isBar Bar = true
isBar _ = false

isArea ∷ ChartType → Boolean
isArea Area = true
isArea _ = false

isScatter ∷ ChartType → Boolean
isScatter Scatter = true
isScatter _ = false

isRadar ∷ ChartType → Boolean
isRadar Radar = true
isRadar _ = false

isFunnel ∷ ChartType → Boolean
isFunnel Funnel = true
isFunnel _ = false

isGraph ∷ ChartType → Boolean
isGraph Graph = true
isGraph _ = false

parseChartType ∷ String → Either String ChartType
parseChartType "pie" = pure Pie
parseChartType "line" = pure Line
parseChartType "bar" = pure Bar
parseChartType "area" = pure Area
parseChartType "scatter" = pure Scatter
parseChartType "radar" = pure Radar
parseChartType "funnel" = pure Funnel
parseChartType "graph" = pure Graph
parseChartType _ = Left "incorrect chartType"

printChartType ∷ ChartType → String
printChartType Pie = "pie"
printChartType Line = "line"
printChartType Bar = "bar"
printChartType Area = "area"
printChartType Scatter = "scatter"
printChartType Radar = "radar"
printChartType Funnel = "funnel"
printChartType Graph = "graph"

derive instance genericChartType ∷ Generic ChartType
derive instance eqChartType ∷ Eq ChartType
derive instance ordChartType ∷ Ord ChartType

instance encodeJsonChartType ∷ EncodeJson ChartType where
  encodeJson = fromString ∘ printChartType

instance decodeJsonChartType ∷ DecodeJson ChartType where
  decodeJson json = decodeJson json >>= parseChartType

instance arbitraryChartType ∷ SC.Arbitrary ChartType where
  arbitrary = Gen.elements Pie $ L.fromFoldable
    [ Pie
    , Line
    , Bar
    , Area
    , Scatter
    , Radar
    , Funnel
    , Graph
    ]
