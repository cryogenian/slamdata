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

module SlamData.Workspace.Card.Setups.Chart.Boxplot.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Boxplot.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array ((!!))
import Data.Array as A
import Data.List as L
import Data.List ((:))
import Data.Lens ((.~))
import Data.Map as M
import Data.Int as Int
import Data.Set as Set
import Data.NonEmpty as NE

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Boxplot))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning (rectangularGrids, rectangularTitles, adjustRectangularPositions)
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

import Utils.Foldable (enumeratedFor_)

type Item =
  { dimension ∷ String
  , value ∷ Number
  , series ∷ Maybe String
  , parallel ∷ Maybe String
  }

type OnOneBoxplot =
  { name ∷ Maybe String
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , fontSize ∷ Maybe Int
  , series ∷ Array BoxplotSeries
  }

type BoxplotSeries =
  { name ∷ Maybe String
  , items ∷ String >> (Array Number × BoxplotItem)
  }

type BoxplotItem = Maybe
  { low ∷ Number
  , q1 ∷ Number
  , q2 ∷ Number
  , q3 ∷ Number
  , high ∷ Number
  }

boundIQR ∷ Maybe Number
boundIQR = Just 1.5

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  dimension ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "dimension"
  value ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "value"
  series ← map Sem.maybeString $ obj .? "series"
  parallel ← map Sem.maybeString $ obj .? "parallel"
  pure { dimension
       , value
       , series
       , parallel
       }

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval m =
  BCE.chartSetupEval
    (\md fp →
      addOrderBy $ SCC.buildBasicSql buildProjections (const Nothing) md fp
    ) buildBoxplot m
  where
  fields ∷ ModelR → L.List Sql.Sql
  fields r =
    L.fromFoldable $ A.catMaybes
      [ r.parallel <#> SCC.jcursorSql
      , r.series <#> SCC.jcursorSql
      , Just $ r.dimension # SCC.jcursorSql
      ]
  orderBy ∷ Maybe (Sql.OrderBy Sql.Sql)
  orderBy = m >>= \r → case fields r of
    L.Nil → Nothing
    x : xs → Just $ Sql.OrderBy $ Tuple Sql.ASC <$> NE.NonEmpty x xs

  addOrderBy ∷ Sql.Sql → Sql.Sql
  addOrderBy =
    (Sql._Select ∘ Sql._orderBy .~ orderBy)
    ∘ (Sql._Select ∘ Sql._isDistinct .~ true)

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.value # SCC.jcursorPrj # Sql.as "value"
  , r.dimension # SCC.jcursorPrj # Sql.as "dimension"
  , r.series # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "series"
  , r.parallel # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "parallel"
  ]

buildBoxplot ∷ ModelR → Axes → Port.Port
buildBoxplot m axes =
  Port.ChartInstructions
    { options: boxplotOptions m axes ∘ buildBoxplotData
    , chartType: Boxplot
    }

buildBoxplotData ∷ JArray → Array OnOneBoxplot
buildBoxplotData =
  adjustRectangularPositions
  ∘ oneBoxplots
  ∘ foldMap (foldMap A.singleton ∘ decodeItem)
  where
  oneBoxplots ∷ Array Item → Array OnOneBoxplot
  oneBoxplots =
    BCE.groupOn _.parallel
      ⋙ map \(name × is) →
        { name
        , x: Nothing
        , y: Nothing
        , w: Nothing
        , h: Nothing
        , fontSize: Nothing
        , series: boxplotSeries is
        }

  boxplotSeries ∷ Array Item → Array BoxplotSeries
  boxplotSeries =
    BCE.groupOn _.series
      ⋙ map \(name × is) →
        { name
        , items: boxplotPoints is
        }

  boxplotPoints ∷ Array Item → String >> (Array Number × BoxplotItem)
  boxplotPoints =
    M.fromFoldable
    ∘ map (map analyzeBoxplot)
    ∘ BCE.groupOn _.dimension

  analyzeBoxplot ∷ Array Item → Array Number × BoxplotItem
  analyzeBoxplot is
    | A.null is = [] × Nothing
    | otherwise =
      let
        arr ∷ Array Number
        arr = map _.value is

        sortedArr ∷ Array Number
        sortedArr = A.sort arr

        quantile ∷ Array Number → Number → Maybe Number
        quantile inp p =
          let
            quantileItemIxNum =
              Int.toNumber (A.length inp - one) * p + one
            quantileItemIxInt =
              Int.floor quantileItemIxNum
            quantileItemBottom =
              inp !! (quantileItemIxInt - one)
            quantileItemTop =
              inp !! quantileItemIxInt
            ixesDiff =
              quantileItemIxNum - Int.toNumber quantileItemIxInt
          in if ixesDiff > 0.0
             then case quantileItemBottom × quantileItemTop of
               Just bItem × Just tItem → Just $ bItem + ixesDiff * (tItem - bItem)
               _ → Nothing
             else quantileItemBottom

        q1 = fromMaybe zero $ quantile sortedArr 0.25
        q2 = fromMaybe zero $ quantile sortedArr 0.5
        q3 = fromMaybe zero $ quantile sortedArr 0.75
        iqr = q3 - q1
        low = case boundIQR of
          Just b → q1 - b * iqr
          _ → fromMaybe zero $ A.head sortedArr
        high = case boundIQR of
          Just b → q3 + b * iqr
          _ → fromMaybe zero $ A.last sortedArr
        outliers ∷ Array Number
        outliers = A.filter (\x → x < low ∨ x > high) sortedArr
      in
       outliers × Just {low, q1, q2, q3, high}


boxplotOptions ∷ ModelR → Axes → Array OnOneBoxplot → DSL OptionI
boxplotOptions r axes boxplotData = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  rectangularTitles boxplotData
    $ maybe "" D.jcursorLabel r.parallel

  rectangularGrids boxplotData


  E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem serieNames

  E.xAxes xAxes

  E.yAxes yAxes

  E.series series

  where
  serieNames ∷ Array String
  serieNames =
    A.fromFoldable
    $ flip foldMap boxplotData
    $ foldMap (Set.fromFoldable ∘ _.name)
    ∘ _.series

  series = enumeratedFor_ boxplotData \(ix × onOnePlot) → for_ onOnePlot.series \serie → do
    E.boxPlot $ boxplotSerie $ ix × serie
    E.scatter $ scatterSerie $ ix × serie

  boxplotSerie (ix × serie) = do
    for_ serie.name E.name

    E.xAxisIndex ix
    E.yAxisIndex ix

    E.itemStyle $ E.normal do
      E.borderWidth 2
      E.borderColor
        $ fromMaybe (C.rgba 0 0 0 0.5)
        $ serie.name
        >>= flip A.elemIndex serieNames
        >>= (colors !! _)

    E.tooltip $ E.formatterItem \item →
      CCT.tableRows
        [ D.jcursorLabel r.dimension × item.name
        , "Upper" × CCT.formatNumberValueIx 4 item
        , "Q3" × CCT.formatNumberValueIx 3 item
        , "Median" × CCT.formatNumberValueIx 2 item
        , "Q1" × CCT.formatNumberValueIx 1 item
        , "Lower" × CCT.formatNumberValueIx 0 item
        ]

    E.buildItems
      $ for_ xAxisLabels \key → case M.lookup key serie.items of
        Nothing → E.missingItem
        Just (_ × mbBP) → for_ mbBP \item → E.addItem $ E.buildValues do
          E.addValue item.low
          E.addValue item.q1
          E.addValue item.q2
          E.addValue item.q3
          E.addValue item.high


  scatterSerie (ix × serie) = do
    for_ serie.name E.name
    E.xAxisIndex ix
    E.yAxisIndex ix
    E.symbolSize
      if isNothing serie.name ∨ serie.name ≡ Just ""
        then 5
        else 0
    E.itemStyle $ E.normal
      $ E.color
      $ fromMaybe (C.rgba 0 0 0 0.5)
      $ serie.name
      >>= flip A.elemIndex serieNames
      >>= (colors !! _)

    E.tooltip $ E.formatterItem\param →
      param.name ⊕ "<br/>"
      ⊕ CCT.formatNumberValueIx 0 param

    E.buildItems
      $ enumeratedFor_ serie.items \(ox × (outliers × _)) →
          for_ outliers \outlier → E.addItem $ E.buildValues do
            E.addValue $ Int.toNumber ox
            E.addValue outlier

  grids ∷ Array (DSL ETP.GridI)
  grids = boxplotData <#> \{x, y, w, h} → do
    for_ x $ E.left ∘ ET.Percent
    for_ y $ E.top ∘ ET.Percent
    for_ w E.widthPct
    for_ h E.heightPct

  titles ∷ Array (DSL ETP.TitleI)
  titles = boxplotData <#> \{x, y, name, fontSize} → do
    for_ name E.text
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      for_ fontSize E.fontSize
    for_ x $ E.left ∘ ET.Percent
    for_ y $ E.top ∘ ET.Percent
    E.textCenter
    E.textMiddle

  xAxisLabels ∷ Array String
  xAxisLabels =
    A.fromFoldable
    $ flip foldMap boxplotData
    $ foldMap (Set.fromFoldable ∘ M.keys ∘ _.items)
    ∘ _.series

  xAxes = enumeratedFor_ boxplotData \(ix × _) → E.addXAxis do
    E.gridIndex ix
    E.axisType ET.Category
    E.axisLabel do
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.splitArea E.hidden
    E.items $ map ET.strItem xAxisLabels


  yAxes = enumeratedFor_ boxplotData \(ix × _) → E.addYAxis do
    E.gridIndex ix
    E.axisType ET.Value
    E.axisLabel do
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.splitArea E.hidden
