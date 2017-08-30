{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Viz.Eval.Parallel where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array as A
import Data.Lens ((?~))
import Data.StrMap as Sm
import Data.String as S
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Viz.Model as M
import SqlSquared as Sql
import Utils.SqlSquared (all, asRel, variRelation)

type Item =
  { series ∷ String
  , dims ∷ Array Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  series ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "series"
  let
    ks ∷ Array String
    ks = map ("measure" ⊕ _) $ A.mapMaybe (S.stripPrefix $ S.Pattern "measure") $ Sm.keys obj

  dims ← for ks \k →
    map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? k

  pure { dims, series }

eval
  ∷ ∀ m
  . VizEval m
  ( M.ChartModel
  → Port.ChartInstructionsPort
  → m ( Port.Resource × DSL OptionI )
  )
eval m { chartType, dimMap, aux, axes } = do
  var × resource ← CEM.extractResourcePair Port.Initial

  let
    sql = buildSql (M.getEvents m) var

  CEM.CardEnv { path, varMap } ← ask

  outResource ←
    CE.liftQ $ CEC.localEvalResource (Sql.Query empty sql) varMap
  records ←
    CE.liftQ $ CEC.sampleResource path outResource Nothing

  let
    items = buildData records
    options = buildOptions dimMap items
  pure $ outResource × options

buildSql ∷ Array M.FilteredEvent → Port.Var → Sql.Sql
buildSql es var =
  Sql.buildSelect
  $ all
  ∘ (Sql._relations
     ?~ (variRelation (unwrap var) # asRel "res"))

buildData ∷ JArray → Array Item
buildData =
  foldMap $ foldMap A.singleton ∘ decodeItem

buildOptions ∷ P.DimMap → Array Item → DSL OptionI
buildOptions dimMap pData = do
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
  serieNames = map _.series pData

  series = for_ pData \serie → E.parallelSeries do
    E.name serie.series
    E.buildItems
      $ E.addItem
      $ E.buildValues
      $ for_ serie.dims E.addValue

  axes = for_ (P.dims dimMap) \(dimIx × dim) → E.addParallelAxis do
    E.dim dimIx
    E.name $ D.jcursorLabel dim
