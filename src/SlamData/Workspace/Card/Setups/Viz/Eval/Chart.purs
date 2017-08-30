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

module SlamData.Workspace.Card.Setups.Viz.Eval.Chart
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.State (get)
import Data.Array as A
import Data.Lens ((.~), (?~))
import Data.List ((:))
import Data.List as L
import Data.NonEmpty as NE
import Data.Variant as V
import SlamData.Workspace.Card.CardType.Chart as Cht
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Common.Sql (numFuncs)
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SqlSquared as Sql

type ChartEval m = VizEval m (Port.Port → P.DimMap → m Port.Resource)

eval ∷ ∀ m. VizEval m (Cht.Chart () → ChartEval m)
eval cht =
  cht # V.match
    { area: const $ evalArea
    , line: const $ evalLine
    , bar: const $ evalBar
    , pie: const $ evalPie
    , scatter: const $ evalScatter
    , radar: const $ evalRadar
    , funnel: const $ evalFunnel
    , graph: const $ evalGraph
    , heatmap: const $ evalHeatmap
    , sankey: const $ evalSankey
    , gauge: const $ evalGauge
    , boxplot: const $ evalBoxplot
    , metric: const $ evalMetric
    , punchCard: const $ evalPunchCard
    , candlestick: const $ evalCandlestick
    , parallel: const $ evalParallel
    , pivot: const $ evalPivot
    }

chartGrouppingEval ∷ ∀ m. ( Port.Var → P.DimMap → Sql.Sql ) → ChartEval m
chartGrouppingEval buildSql port dimMap = do
  var × resource ← CEM.extractResourcePair port
  records × axes ← BCE.analyze resource =<< get
  CEM.CardEnv { varMap, cardId } ← ask
  let
    sql = buildSql var dimMap
  CE.liftQ $ CEC.localEvalResource (Sql.Query numFuncs sql) varMap

buildBasicSql
  ∷ ( P.DimMap → L.List (Sql.Projection Sql.Sql) )
  → ( P.DimMap → Maybe (Sql.GroupBy Sql.Sql) )
  → Port.Var
  → P.DimMap
  → Sql.Sql
buildBasicSql buildProjections buildGroupBy (Port.Var vari) dimMap =
  Sql.buildSelect
    $ ( Sql._projections .~ buildProjections dimMap )
    ∘ ( Sql._relations ?~ Sql.VariRelation { vari, alias: Nothing })
    ∘ ( Sql._groupBy .~ groups )
    ∘ ( Sql._orderBy .~ orders )
  where
  groups = buildGroupBy dimMap
  orders = groups >>= SC.groupByToOrderBy

evalArea ∷ ∀ m. ChartEval m
evalArea =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.measureProjection P.value dimMap "measure"
    , SC.dimensionProjection P.dimension dimMap "dimension"
    , SC.dimensionProjection P.series dimMap "series"
    ]
  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.series dimMap
    <|> SC.sqlProjection P.dimension dimMap

evalLine ∷ ∀ m. ChartEval m
evalLine =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.dimension dimMap "dimension"
    , SC.measureProjection P.value dimMap "measure1"
    , SC.measureProjection P.size dimMap "size"
    , SC.measureProjection P.secondValue dimMap "measure2"
    , SC.dimensionProjection P.series dimMap "series"
    ]
  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.series dimMap
    <|> SC.sqlProjection P.dimension dimMap

evalBar ∷ ∀ m. ChartEval m
evalBar =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.measureProjection P.value dimMap "measure"
    , SC.dimensionProjection P.category dimMap "category"
    , SC.dimensionProjection P.stack dimMap "stack"
    , SC.dimensionProjection P.parallel dimMap "parallel"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.parallel dimMap
    <|> SC.sqlProjection P.stack dimMap
    <|> SC.sqlProjection P.category dimMap

evalPie ∷ ∀ m. ChartEval m
evalPie =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.category dimMap "category"
    , SC.measureProjection P.value dimMap "measure"
    , SC.dimensionProjection P.donut dimMap "donut"
    , SC.dimensionProjection P.parallel dimMap "parallel"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.parallel dimMap
    <|> SC.sqlProjection P.donut dimMap
    <|> SC.sqlProjection P.category dimMap

evalScatter ∷ ∀ m. ChartEval m
evalScatter =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.abscissa dimMap "abscissa"
    , SC.dimensionProjection P.ordinate dimMap "ordinate"
    , SC.measureProjection P.size dimMap "size"
    , SC.dimensionProjection P.parallel dimMap "parallel"
    , SC.dimensionProjection P.series dimMap "series"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.parallel dimMap
    <|> SC.sqlProjection P.series dimMap
    <|> SC.sqlProjection P.abscissa dimMap

evalRadar ∷ ∀ m. ChartEval m
evalRadar =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.category dimMap "category"
    , SC.measureProjection P.value dimMap "measure"
    , SC.dimensionProjection P.multiple dimMap "multiple"
    , SC.dimensionProjection P.parallel dimMap "parallel"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.parallel dimMap
    <|> SC.sqlProjection P.multiple dimMap
    <|> SC.sqlProjection P.category dimMap

evalFunnel ∷ ∀ m. ChartEval m
evalFunnel =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.category dimMap "category"
    , SC.measureProjection P.value dimMap "measure"
    , SC.dimensionProjection P.series dimMap "series"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.series dimMap
    <|> SC.sqlProjection P.category dimMap

evalGraph ∷ ∀ m. ChartEval m
evalGraph =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.source dimMap "source"
    , SC.dimensionProjection P.target dimMap "target"
    , SC.measureProjection P.size dimMap "size"
    , SC.dimensionProjection P.color dimMap "color"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.source dimMap
    <|> SC.sqlProjection P.target dimMap
    <|> SC.sqlProjection P.color dimMap

evalHeatmap ∷ ∀ m. ChartEval m
evalHeatmap =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.abscissa dimMap "abscissa"
    , SC.dimensionProjection P.ordinate dimMap "ordinate"
    , SC.measureProjection P.value dimMap "measure"
    , SC.dimensionProjection P.series dimMap "series"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.series dimMap
    <|> SC.sqlProjection P.abscissa dimMap
    <|> SC.sqlProjection P.ordinate dimMap

evalSankey ∷ ∀ m. ChartEval m
evalSankey =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.source dimMap "source"
    , SC.dimensionProjection P.target dimMap "target"
    , SC.measureProjection P.value dimMap "weight"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.source dimMap
    <|> SC.sqlProjection P.target dimMap

evalGauge ∷ ∀ m. ChartEval m
evalGauge =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.measureProjection P.value dimMap "measure"
    , SC.dimensionProjection P.multiple dimMap "multiple"
    , SC.dimensionProjection P.parallel dimMap "parallel"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.parallel dimMap
    <|> SC.sqlProjection P.multiple dimMap

evalBoxplot ∷ ∀ m. ChartEval m
evalBoxplot =
  chartGrouppingEval \var dimMap →
    addOrderBy dimMap $ buildBasicSql buildProjections buildGroupBy var dimMap
  where
  addOrderBy ∷ P.DimMap → Sql.Sql → Sql.Sql
  addOrderBy dimMap  =
    (Sql._Select ∘ Sql._orderBy .~ orderBy dimMap)
    ∘ (Sql._Select ∘ Sql._isDistinct .~ true)

  fields ∷ P.DimMap → L.List Sql.Sql
  fields dimMap =
    SC.sqlProjection P.parallel dimMap
    <|> SC.sqlProjection P.series dimMap
    <|> SC.sqlProjection P.dimension dimMap

  orderBy ∷ P.DimMap → Maybe (Sql.OrderBy Sql.Sql)
  orderBy dimMap  = case fields dimMap of
    L.Nil → Nothing
    x : xs → Just $ Sql.OrderBy $ Tuple Sql.ASC <$> NE.NonEmpty x xs

  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.flatValue dimMap "value"
    , SC.dimensionProjection P.dimension dimMap "dimension"
    , SC.dimensionProjection P.series dimMap "series"
    , SC.dimensionProjection P.parallel dimMap "parallel"
    ]

  buildGroupBy _ = Nothing

evalMetric ∷ ∀ m. ChartEval m
evalMetric port _ =
  map snd $ CEM.extractResourcePair port

evalPunchCard ∷ ∀ m. ChartEval m
evalPunchCard =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.abscissa dimMap "abscissa"
    , SC.dimensionProjection P.ordinate dimMap "ordinate"
    , SC.measureProjection P.value dimMap "measure"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.abscissa dimMap
    <|> SC.sqlProjection P.ordinate dimMap

evalCandlestick ∷ ∀ m. ChartEval m
evalCandlestick =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  buildProjections dimMap = L.fromFoldable $ A.concat
    [ SC.dimensionProjection P.dimension dimMap "dimension"
    , SC.measureProjection P.high dimMap "high"
    , SC.measureProjection P.low dimMap "low"
    , SC.measureProjection P.open dimMap "open"
    , SC.measureProjection P.close dimMap "close"
    , SC.dimensionProjection P.parallel dimMap "parallel"
    ]

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.parallel dimMap
    <|> SC.sqlProjection P.dimension dimMap

evalParallel ∷ ∀ m. ChartEval m
evalParallel =
  chartGrouppingEval $ buildBasicSql buildProjections buildGroupBy
  where
  mkProjection dimMap (ix × _) = [ SC.measureProjection (P.dimIx ix) dimMap $ "measure" ⊕ show ix ]

  buildProjections dimMap = L.fromFoldable $ A.concat
    $ [ SC.dimensionProjection P.series dimMap "series" ]
    ⊕ ( foldMap (mkProjection dimMap) $ P.dims dimMap )

  buildGroupBy dimMap = SC.groupBy
    $ SC.sqlProjection P.series dimMap

evalPivot ∷ ∀ m. ChartEval m
evalPivot port _  =
  map snd $ CEM.extractResourcePair port
