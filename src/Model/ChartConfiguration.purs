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

module Model.ChartConfiguration where

import Prelude

import Data.Argonaut
  (JCursor(), EncodeJson, DecodeJson, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Array (filter, cons)
import Data.Foldable (any)
import Data.Lens (LensP(), lens, (^.))
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Model.Aggregation (Aggregation())
import Model.ChartAxis (dependsOn)
import Model.Select (Select(), _value)

type JSelect = Select JCursor

depends :: JSelect -> Array JCursor -> Array JCursor
depends sel lst = maybe lst go (sel ^. _value)
  where
  go y = filter (dependsOn y) lst

dependsOnArr :: Array JCursor -> Array JCursor -> Array JCursor
dependsOnArr dependency arr =
  filter (flip any dependency <<< dependsOn) arr

type ChartConfigurationR =
 { series :: Array JSelect
 , dimensions :: Array JSelect
 , measures :: Array JSelect
 , aggregations :: Array (Select Aggregation)
 }

newtype ChartConfiguration = ChartConfiguration ChartConfigurationR

_ChartConfiguration :: LensP ChartConfiguration ChartConfigurationR
_ChartConfiguration = lens (\(ChartConfiguration r) -> r) (const ChartConfiguration)

_series :: LensP ChartConfiguration (Array JSelect)
_series = _ChartConfiguration <<< lens _.series _{series = _}

_dimensions :: LensP ChartConfiguration (Array JSelect)
_dimensions = _ChartConfiguration <<< lens _.dimensions _{dimensions = _}

_measures :: LensP ChartConfiguration (Array JSelect)
_measures = _ChartConfiguration <<< lens _.measures _{measures = _}

_aggregations :: LensP ChartConfiguration (Array (Select Aggregation))
_aggregations = _ChartConfiguration <<< lens _.aggregations _{aggregations = _}

instance encodeJsonChartConfiguration :: EncodeJson ChartConfiguration where
  encodeJson (ChartConfiguration r) =
       "series" := r.series
    ~> "dimensions" := r.dimensions
    ~> "measures" := r.measures
    ~> "aggregations" := r.aggregations
    ~> jsonEmptyObject
instance decodeJsonChartConfiguration :: DecodeJson ChartConfiguration where
  decodeJson json = do
    obj <- decodeJson json
    r <- { series: _, dimensions: _, measures: _, aggregations: _}
         <$> (obj .? "series")
         <*> (obj .? "dimensions")
         <*> (obj .? "measures")
         <*> (obj .? "aggregations")
    pure $ ChartConfiguration r
