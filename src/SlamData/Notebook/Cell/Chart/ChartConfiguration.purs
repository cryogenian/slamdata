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

module SlamData.Notebook.Cell.Chart.ChartConfiguration where

import Prelude

import Control.Bind ((>=>))

import Data.Argonaut (Json(), JCursor(), (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Array (filter)
import Data.Either (Either())
import Data.Foldable (any)
import Data.Lens ((^.))
import Data.Maybe (maybe)

import SlamData.Form.Select (Select(), _value)
import SlamData.Notebook.Cell.Chart.Aggregation (Aggregation())
import SlamData.Notebook.Cell.Chart.Axis (dependsOn)

type JSelect = Select JCursor

depends :: JSelect -> Array JCursor -> Array JCursor
depends sel lst = maybe lst go (sel ^. _value)
  where
  go y = filter (dependsOn y) lst

dependsOnArr :: Array JCursor -> Array JCursor -> Array JCursor
dependsOnArr dependency arr =
  filter (flip any dependency <<< dependsOn) arr

type ChartConfiguration =
 { series :: Array JSelect
 , dimensions :: Array JSelect
 , measures :: Array JSelect
 , aggregations :: Array (Select Aggregation)
 }

encode :: ChartConfiguration -> Json
encode r
   = "series" := r.series
  ~> "dimensions" := r.dimensions
  ~> "measures" := r.measures
  ~> "aggregations" := r.aggregations
  ~> jsonEmptyObject

decode :: Json -> Either String ChartConfiguration
decode = decodeJson >=> \obj -> do
  { series: _, dimensions: _, measures: _, aggregations: _}
    <$> obj .? "series"
    <*> obj .? "dimensions"
    <*> obj .? "measures"
    <*> obj .? "aggregations"
