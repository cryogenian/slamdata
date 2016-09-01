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

module SlamData.Workspace.Card.ChartOptions.Eval where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array (cons, length, null, catMaybes)
import Data.Lens ((^?))
import Data.Lens as Lens
import Data.Set as Set
import Data.Map as Map

import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))
import SlamData.Workspace.Card.ChartOptions.Model as ChartOptions
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port


eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ CET.CardEvalInput
  → ChartOptions.Model
  → CET.CardEvalT m Port.ChartPort
eval info model = do
  resource ←
    info.input
      ^? Lens._Just ∘ Port._Resource
      # maybe (QE.throw "Expected Resource input") pure

  numRecords ← CET.liftQ $ QQ.count resource

  when (numRecords > 10000)
    $ QE.throw
      $ "The 10000 record limit for visualizations has been exceeded - the current dataset contains "
      ⊕ show numRecords
      ⊕ " records. "
      ⊕ "Please consider using a 'limit' or 'group by' clause in the query to reduce the result size."

  recordSample ←
    QQ.sample resource 0 20
      # lift
      >>= either (const $ QE.throw "Error getting input resource") pure

  when (null recordSample)
    $ QE.throw "Input resource is empty"

  let
    sample = analyzeJArray recordSample
    axes = getAxes sample
    available =
      catMaybes
        $ map (\x → x axes)
        [ getMaybePie
        , getMaybeBar
        , getMaybeLine
        , getMaybeArea
        , getMaybeScatter
        , getMaybeRadar
        , getMaybeFunnel
        , getMaybeGraph
        , getMaybeHeatmap
        , getMaybeSankey
        , getMaybeGauge
        ]

  when (null available)
    $ QE.throw "There is no available chart types for this data"

  let
    availableChartTypes = foldMap Set.singleton available

  -- Commented till this it would be clear how this should work exactly
  -- see https://github.com/slamdata/slamdata/issues/931
  --  when (isNothing model.chartConfig)
  --    $ EC.throwError "Please select axes"

  pure
    { resource
    , availableChartTypes
    , axes
    , config: model
    }
  where
  getAxes ∷ Map.Map JCursor Axis → Axes
  getAxes sample =
    foldl foldFn {category: [], value: [], time: []} $ Map.toList sample

  foldFn ∷ Axes → JCursor × Axis → Axes
  foldFn accum (cursor × axis)
    | Ax.isCatAxis axis = accum { category = cons cursor accum.category }
    | Ax.isValAxis axis = accum { value = cons cursor accum.value }
    | Ax.isTimeAxis axis = accum { time = cons cursor accum.time }
    | otherwise = accum

  getMaybePie ∷ Axes → Maybe ChartType
  getMaybePie axes = do
    guard $ (not $ null axes.value) ∧ (not $ null axes.category)
    pure Pie

  getMaybeBar ∷ Axes → Maybe ChartType
  getMaybeBar axes = do
    guard $ (not $ null axes.value) ∧ (not $ null axes.category)
    pure Bar

  getMaybeLine ∷ Axes → Maybe ChartType
  getMaybeLine axes = do
    guard $ (not $ null axes.value) ∧ ((not $ null axes.category) || (not $ null axes.time))
    pure Line

  getMaybeArea ∷ Axes → Maybe ChartType
  getMaybeArea axes = do
    guard $ (not $ null axes.value) ∧ ((not $ null axes.category) || (not $ null axes.time))
    pure Area

  getMaybeScatter ∷ Axes → Maybe ChartType
  getMaybeScatter axes = do
    guard $ length axes.value >= 2
    pure Scatter

  getMaybeRadar ∷ Axes → Maybe ChartType
  getMaybeRadar axes = do
    guard $ (not $ null axes.value) ∧ ((not $ null axes.category) || (not $ null axes.time))
    pure Radar

  getMaybeFunnel ∷ Axes → Maybe ChartType
  getMaybeFunnel axes = do
    guard $ (not $ null axes.value) ∧ ((not $ null axes.category) || (not $ null axes.time))
    pure Funnel

  getMaybeGraph ∷ Axes → Maybe ChartType
  getMaybeGraph axes = do
    guard $ (length axes.category > 1)
    pure Graph

  getMaybeHeatmap ∷ Axes → Maybe ChartType
  getMaybeHeatmap axes = do
    guard
      $ (not $ null axes.value)
      ∧ (((length axes.category) + (length axes.time) + (length axes.value)) > 2)
    pure Heatmap

  getMaybeSankey ∷ Axes → Maybe ChartType
  getMaybeSankey axes = do
    guard $ (length axes.category > 1 ∧ length axes.value > 0)
    pure Sankey

  getMaybeGauge ∷ Axes → Maybe ChartType
  getMaybeGauge axes = do
    guard $ (length axes.category > 0 ∧ length axes.value > 0)
    pure Gauge
