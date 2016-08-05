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

module SlamData.Workspace.Card.Chart.ChartConfiguration
  ( JSelect
  , depends
  , dependsOnArr
  , ChartConfiguration
  , eqChartConfiguration
  , genChartConfiguration
  , encode
  , decode
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, JCursor, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Argonaut.JCursor as JC
import Data.Array (filter)
import Data.Foldable (any)
import Data.Lens ((^.))

import SlamData.Form.Select (Select, _value)
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, runArbAggregation)
import SlamData.Workspace.Card.Chart.Axis (dependsOn)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen


type JSelect = Select JCursor

depends ∷ JSelect →  Array JCursor →  Array JCursor
depends sel lst = maybe lst go (sel ^. _value)
  where
  go y = filter (dependsOn y) lst

dependsOnArr ∷ Array JCursor →  Array JCursor →  Array JCursor
dependsOnArr dependency arr =
  filter (flip any dependency ∘ dependsOn) arr

type ChartConfiguration =
  { series ∷ Array JSelect
  , dimensions ∷ Array JSelect
  , measures ∷ Array JSelect
  , aggregations ∷ Array (Select (Maybe Aggregation))
  }

eqChartConfiguration ∷ ChartConfiguration → ChartConfiguration → Boolean
eqChartConfiguration c1 c2 =
  c1.series ≡ c2.series
    && c1.dimensions ≡ c2.dimensions
    && c1.measures ≡ c2.measures
    && c1.aggregations ≡ c2.aggregations

newtype ArbJCursor = ArbJCursor JCursor

runArbJCursor ∷ ArbJCursor → JCursor
runArbJCursor (ArbJCursor x) = x

-- TODO: proper (sized) implementation -js
instance arbitraryArbJCursor ∷ SC.Arbitrary ArbJCursor where
  arbitrary = pure $ ArbJCursor JC.JCursorTop

genChartConfiguration ∷ Gen.Gen ChartConfiguration
genChartConfiguration = do
  series ← Gen.arrayOf genJSelect
  dimensions ← Gen.arrayOf genJSelect
  measures ← Gen.arrayOf genJSelect
  aggregations ← map (map runArbAggregation) <$> SC.arbitrary
  pure { series, dimensions, measures, aggregations }
  where
    genJSelect ∷ Gen.Gen JSelect
    genJSelect = map runArbJCursor <$> SC.arbitrary

encode ∷ ChartConfiguration →  Json
encode r
   = "series" := r.series
  ~> "dimensions" := r.dimensions
  ~> "measures" := r.measures
  ~> "aggregations" := r.aggregations
  ~> jsonEmptyObject

decode ∷ Json →  Either String ChartConfiguration
decode = decodeJson >=> \obj → do
  { series: _, dimensions: _, measures: _, aggregations: _}
    <$> obj .? "series"
    <*> obj .? "dimensions"
    <*> obj .? "measures"
    <*> ((obj .? "aggregations") <|> ((obj .? "aggregations") <#> map (map Just)))
