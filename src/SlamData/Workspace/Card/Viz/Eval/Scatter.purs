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

module SlamData.Workspace.Card.Viz.Eval.Scatter where

import SlamData.Prelude

import Color as C
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.Lens ((?~))
import Data.Set as Set
import Data.Variant as V
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP
import Global (infinity)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Auxiliary.Scatter as Scatter
import SlamData.Workspace.Card.Setups.Chart.Common.Brush as CCB
import SlamData.Workspace.Card.Setups.ColorScheme (colors, getTransparentColor)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Viz.Model as M
import SqlSquared as Sql
import Utils.Foldable (enumeratedFor_)
import Utils.SqlSquared (all, asRel, variRelation)

type ScatterSeries =
  { name ∷ Maybe String
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , fontSize ∷ Maybe Int
  , series ∷ Array OnOneGrid
  }

type ScatterItem =
  { x ∷ Number
  , y ∷ Number
  , r ∷ Number
  , size ∷ Int
  }

type OnOneGrid =
  { name ∷ Maybe String
  , items ∷ Array ScatterItem
  }

type Item =
  { abscissa ∷ Number
  , ordinate ∷ Number
  , size ∷ Number
  , parallel ∷ Maybe String
  , series ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  abscissa ← Sem.requiredNumber zero <$> obj .? "abscissa"
  ordinate ← Sem.requiredNumber zero <$> obj .? "ordinate"
  size ← Sem.requiredNumber zero <$> obj .? "size"
  parallel ← Sem.maybeString <$> obj .? "parallel"
  series ← Sem.maybeString <$> obj .? "series"
  pure { abscissa, ordinate, size, parallel, series }

eval
  ∷ ∀ m
  . VizEval m
  ( M.ChartModel
  → Port.ChartInstructionsPort
  → m ( Port.Resource × DSL OptionI )
  )
eval m { chartType, dimMap, aux } = do
  var × resource ← CEM.extractResourcePair Port.Initial

  aux' ←
    maybe
      (CE.throw "Missing or incorrect auxiliary model, please contact support")
      pure
      $ V.prj CT._scatter =<< aux

  let
    sql = buildSql (M.getEvents m) var

  CEM.CardEnv { path, varMap } ← ask

  outResource ←
    CE.liftQ $ CEC.localEvalResource (Sql.Query empty sql) varMap
  records ←
    CE.liftQ $ CEC.sampleResource path outResource Nothing

  let
    items = buildData aux' records
    options = buildOptions dimMap aux' items
  pure $ outResource × options

buildSql ∷ Array M.FilteredEvent → Port.Var → Sql.Sql
buildSql es var =
  Sql.buildSelect
  $ all
  ∘ (Sql._relations
     ?~ (variRelation (unwrap var) # asRel "res"))


buildData ∷ Scatter.State → Array Json → Array ScatterSeries
buildData r =
  BCP.adjustRectangularPositions
  ∘ series
  ∘ foldMap (foldMap A.singleton ∘ decodeItem)
  where
  series ∷ Array Item → Array ScatterSeries
  series =
    BCE.groupOn _.parallel
      >>> map \(name × is) →
            { name
            , x: Nothing
            , y: Nothing
            , w: Nothing
            , h: Nothing
            , fontSize: Nothing
            , series: onOneGrids is
            }

  onOneGrids ∷ Array Item → Array OnOneGrid
  onOneGrids =
    BCE.groupOn _.series
      >>> map \(name × is) →
            { name
            , items: adjustSymbolSizes $ toPoint <$> is
            }

  toPoint ∷ Item → ScatterItem
  toPoint item =
    { x: item.abscissa
    , y: item.ordinate
    , r: item.size
    , size: zero
    }

  adjustSymbolSizes ∷ Array ScatterItem → Array ScatterItem
  adjustSymbolSizes is =
    let
      values =
        map _.r is
      minValue =
        fromMaybe zero $ F.minimum values
      maxValue =
        fromMaybe infinity $ F.maximum values
      distance =
        maxValue - minValue
      maxSize =
        r.size.max
      minSize =
        r.size.min
      sizeDistance =
        maxSize - minSize

      relativeSize ∷ Number → Number
      relativeSize val
        | distance ≡ zero = val
        | val < 0.0 = 0.0
        | otherwise =
            maxSize - sizeDistance / distance * (maxValue - val)
    in
      map (\x → x{size = Int.floor $ relativeSize x.r}) is

buildOptions ∷ P.DimMap → Scatter.State → Array ScatterSeries → DSL OptionI
buildOptions dimMap r scatterData = do
  let
    mkRow prj value  = P.lookup prj dimMap # foldMap \dim →
      [ { label: D.jcursorLabel dim, value } ]

    cols = A.fold
      [ mkRow P.abscissa $ CCT.formatValueIx 0
      , mkRow P.ordinate $ CCT.formatValueIx 1
      , mkRow P.size $ CCT.formatValueIx 2
      , mkRow P.series _.seriesName
      ]

  CCB.brush

  CCT.tooltip do
    E.formatterItem (CCT.tableFormatter (pure ∘ _.color) cols ∘ pure)
    E.triggerItem
    E.axisPointer do
      E.crossAxisPointer
      E.crossStyle do
        E.color $ C.rgba 170 170 170 0.6
        E.widthNum 0.2
        E.solidLine
  E.colors colors

  BCP.rectangularGrids scatterData
  BCP.rectangularTitles scatterData
    $ maybe "" D.jcursorLabel
    $ P.lookup P.parallel dimMap

  E.grid BCP.cartesian
  E.xAxes $ valueAxes E.addXAxis
  E.yAxes $ valueAxes E.addYAxis

  E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem seriesNames

  E.series series

  where
  valueAxes ∷ ∀ i a. (DSL (ETP.AxisI (gridIndex ∷ ETP.I|i)) → DSL a) → DSL a
  valueAxes addAxis = enumeratedFor_ scatterData \(ix × _) → addAxis do
    E.gridIndex ix
    E.axisType ET.Value
    E.axisLabel $ E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.color $ C.rgba 184 184 184 1.0
      E.width 1
    E.splitLine $ E.lineStyle do
      E.color $ C.rgba 204 204 204 0.2
      E.width 1

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
    $ foldMap (_.series ⋙ foldMap (_.name ⋙ Set.fromFoldable)) scatterData

  series ∷ ∀ i. DSL (scatter ∷ ETP.I|i)
  series = enumeratedFor_ scatterData \(gridIx × onOneGrid) →
    enumeratedFor_ onOneGrid.series \(ix × serie) → E.scatter do
      E.xAxisIndex gridIx
      E.yAxisIndex gridIx
      for_ serie.name E.name
      for_ (A.index colors $ mod ix $ A.length colors) \color → do
        E.itemStyle $ E.normal $ E.color $ getTransparentColor color 0.5
      E.symbol ET.Circle
      E.buildItems $ for_ serie.items \item → E.addItem do
        E.buildValues do
          E.addValue item.x
          E.addValue item.y
          E.addValue item.r
        when (P.member P.size dimMap) $ E.symbolSize item.size