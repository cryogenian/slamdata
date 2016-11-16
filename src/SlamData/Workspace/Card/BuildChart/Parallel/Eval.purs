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

module SlamData.Workspace.Card.BuildChart.Parallel.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Parallel.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Parallel.Model (ParallelR, Model)
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Parallel))
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

import Utils.Array (enumerate)
import Utils.Foldable (enumeratedFor_)

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Port.TaggedResourcePort
  → Model
  → m Port.Port
eval = BCE.buildChartEval Parallel (const buildParallel)


type Series =
  { name ∷ Maybe String
  , items ∷ Array (Maybe Number)
  }

buildParallelData ∷ ParallelR → JArray → Array Series
buildParallelData r records = series
  where
  -- | maybe series >> data mapped to r.dims
  dataMap ∷ Maybe String >> Array (Array Number)
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> Array (Array Number)
    → Json
    → Maybe String >> Array (Array Number)
  dataMapFoldFn acc js =
    let
      getMaybeString = Sem.getMaybeString js
      getValues = Sem.getValues js

      mbSeries =
        getMaybeString r.series
      values =
        map (getValues ∘ pure) r.dims

      alterSeriesFn
        ∷ Maybe (Array (Array Number))
        → Maybe (Array (Array Number))
      alterSeriesFn Nothing =
        Just values
      alterSeriesFn (Just arrs) =
        Just $ map (uncurry append) $ A.zip arrs values

    in
      M.alter alterSeriesFn mbSeries acc

  series ∷ Array Series
  series =
    foldMap mkSeries $ M.toList dataMap

  mkSeries
    ∷ Maybe String × Array (Array Number)
    → Array Series
  mkSeries (name × values) =
    [ { name
      , items: map aggregateValues $ enumerate values
      } ]

  aggregateValues
    ∷ Int × (Array Number)
    → Maybe Number
  aggregateValues (ix × vals)
    | A.null vals = Nothing
    | otherwise = do
      agg ← r.aggs A.!! ix
      pure $ Ag.runAggregation agg vals



buildParallel ∷ ParallelR → JArray → DSL OptionI
buildParallel r records = do
  E.parallel do
    E.left $ ET.Percent 5.0
    E.right $ ET.Percent 18.0
    E.bottom $ ET.Pixel 100

  E.colors colors
  E.series series

  E.parallelAxes axes

  when (A.length serieNames < 30) $ E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem serieNames

  where
  parallelData ∷ Array Series
  parallelData = buildParallelData r records

  serieNames = A.mapMaybe _.name parallelData

  series = for_ parallelData \serie → E.parallelSeries do
    traverse_ E.name serie.name
    E.buildItems $ E.addItem $ E.buildValues $ for_ serie.items \val →  do
      case val of
        Nothing → E.missingValue
        Just v → E.addValue v

  axes = enumeratedFor_ r.dims \(dimIx × dim) → E.addParallelAxis do
    E.dim dimIx
    E.name $ show dim
