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

module SlamData.Workspace.Card.Chart.BuildOptions.Boxplot where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JCursor)
import Data.Array as A
import Data.Array ((!!))
import Data.Function (on)
import Data.Lens (view)
import Data.List as L
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as M
import Data.Int (toNumber, floor)

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Form.Select (_value)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (ChartAxes, buildChartAxes, SeriesKey, keyName)
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)

import Math ((%))
import Utils (stringToNumber)

-- sample data:
-- Tuple ['dimA', 'dimB']
--       [ Tuple '' [ Tuple 'series1' [Tuple {1,1,1,1,1} [], Tuple {2,2.3,2,5,2.8,3} []]
--                  , Tuple 'series2' [Tuple {1,1,1,1,1} [], Tuple {2,2.3,2,5,2.8,3} []] ] ]
type BoxplotData =
  (Array String) × (Array (String × (Array (String × Array BoxDataAndOutliers))))

type BoxDataAndOutliers = Tuple (Maybe BoxData) (Array Number)

type BoxData = { low ∷ Number, q1 ∷ Number, q2 ∷ Number, q3 ∷ Number, high ∷ Number }

buildBoxplotData ∷ ChartAxes → ET.AxisType → BoxplotData
buildBoxplotData axes dimType = Tuple (A.fromFoldable distinctDims) $
  A.fromFoldable
    $ map (map (A.fromFoldable
      <<< map calculate
      -- output sample:
      -- ( Tuple ''
      --         ( Tuple 'series1' [[1], [2, 3]]
      --         : Tuple 'series2' [[1], [2, 3]] )
      -- )
      <<< map checkDimAndTransform
      -- output sample:
      -- ( Tuple ''
      --         ( Tuple 'series1' (Tuple 'dimA' [1] : Tuple 'dimB' [2, 3])
      --         : Tuple 'series2' (Tuple 'dimA' [1] : Tuple 'dimB' [2, 3]) )
      -- )
      <<< map combineDim
      -- output sample:
      -- ( Tuple ''
      --         ( Tuple 'series1' (Tuple 'dimA' 1 : Tuple 'dimB' 2 : Tuple 'dimB' 3)
      --         : Tuple 'series2' (Tuple 'dimA' 1 : Tuple 'dimB' 2 : Tuple 'dimB' 3) )
      -- )
      <<< L.catMaybes
      <<< map combine
      -- output sample:
      -- ( Tuple ''
      --         ( Tuple 'series1' (Tuple 'dimA' 1) : Tuple 'series1' (Tuple 'dimB' 2) : Tuple 'series1' (Tuple 'dimB' 3)
      --         : Tuple 'series2' (Tuple 'dimA' 1) : Tuple 'series2' (Tuple 'dimB' 2) : Tuple 'series2' (Tuple 'dimB' 3) )
      -- )
      <<< L.groupBy ((==) `on` fst)
      <<< L.sortBy (compare `on` fst)
    ))
  -- output sample:
  -- ( Tuple ''
  --         ( Tuple 'series1' (Tuple 'dimA' 1) : Tuple 'series1' (Tuple 'dimB' 2)
  --         : Tuple 'series1' (Tuple 'dimB' 3) : Tuple 'series2' (Tuple 'dimA' 1)
  --         : Tuple 'series2' (Tuple 'dimB' 2) : Tuple 'series2' (Tuple 'dimB' 3) )
  -- )
  $ L.catMaybes
  $ map combine
  -- output sample:
  -- (( Tuple '' (Tuple 'series1' (Tuple 'dimA' 1)) : Tuple '' (Tuple 'series1' (Tuple 'dimB' 2))
  --  : Tuple '' (Tuple 'series1' (Tuple 'dimB' 3)) : Tuple '' (Tuple 'series2' (Tuple 'dimA' 1))
  --  : Tuple '' (Tuple 'series2' (Tuple 'dimB' 2)) : Tuple '' (Tuple 'series2' (Tuple 'dimB' 3)) ))
  $ L.groupBy ((==) `on` fst)
  $ L.sortBy (compare `on` fst)
  -- output sample:
  -- ( Tuple '' (Tuple 'series1' (Tuple 'dimA' 1)) : Tuple '' (Tuple 'series1' (Tuple 'dimB' 2))
  -- : Tuple '' (Tuple 'series1' (Tuple 'dimB' 3)) : Tuple '' (Tuple 'series2' (Tuple 'dimA' 1))
  -- : Tuple '' (Tuple 'series2' (Tuple 'dimB' 2)) : Tuple '' (Tuple 'series2' (Tuple 'dimB' 3)) )
  $ L.catMaybes
  $ map filterInvalid
  $ tag duplications
  $ tag seriesKeys
  $ L.zip dimensions values

  where
  dimensions ∷ List (Maybe String)
  dimensions = fromMaybe Nil $ axes.dimensions !! 0

  distinctDims ∷ List String
  distinctDims = case dimType of
    ET.Value → L.sortBy (compare `on` stringToNumber) (L.catMaybes $ L.nub dimensions)
    _ → L.sort $ L.catMaybes $ L.nub dimensions

  values ∷ List (Maybe Number)
  values = fromMaybe Nil $ A.index axes.measures 0

  series ∷ List (Maybe String)
  series = fromMaybe Nil $ A.index axes.series 0

  duplications ∷ List (Maybe String)
  duplications = fromMaybe Nil $ A.index axes.series 1

  tag ∷ forall a b. List (Maybe a) → List b → List (Tuple (Maybe a) b)
  tag a b = if L.null a then map (Tuple Nothing) b else L.zip a b

  seriesKeys ∷ List SeriesKey
  seriesKeys = map (\x → mkSeriesKey x Nothing) series

  mkSeriesKey ∷ Maybe String → Maybe String → SeriesKey
  mkSeriesKey f s =
    f >>= \f → pure $ Tuple f s

  filterInvalid
    ∷ (Maybe String) × (SeriesKey × ((Maybe String) × (Maybe Number)))
    → Maybe (String × (String × (String × Number)))
  filterInvalid (d × (s × (v1 × v2))) = case v1 × v2 of
    Just v1' × Just v2' →
      Just $ (fromMaybe "" d) × (keyName (Tuple "" s) × (v1' × v2'))
    _ → Nothing

  combine ∷ forall a b. List (Tuple a b) → Maybe (Tuple a (List b))
  combine x = do
    y ← fst <$> L.head x
    pure $ Tuple y $ map snd x

  combineDim
    ∷ String × List (String × Number)
    → String × List (String × Array Number)
  combineDim x =
    map ( L.catMaybes
      <<< map (map (map A.fromFoldable))
      <<< map combine
      <<< L.groupBy ((==) `on` fst)
      <<< L.sortBy (compare `on` fst)) x

  checkDimAndTransform
    ∷ String × List (String × Array Number)
    → String × Array (Array Number)
  checkDimAndTransform x =
    map (A.fromFoldable <<< matchDim) x
    where
    matchDim ∷ List (String × Array Number) → List (Array Number)
    matchDim x = do
      let m = M.fromFoldable x
      map (fromMaybe [] <<< (flip M.lookup m)) distinctDims

  calculate
    ∷ String × Array (Array Number)
    → String × Array BoxDataAndOutliers
  calculate x =
    -- change
    map (map (boxDataAndOutliers (Just 1.5))) x
    where
    --@see <https://github.com/ecomfe/echarts/blob/master/extension/dataTool/prepareBoxplotData.js>
    boxDataAndOutliers
      ∷ Maybe Number
      → Array Number
      → BoxDataAndOutliers
    boxDataAndOutliers boundIQR a = case A.length a of
      0 → Tuple Nothing []
      _ →
        let
          ascArr = A.sort a
          q1 = fromMaybe zero $ quantile ascArr 0.25
          q2 = fromMaybe zero $ quantile ascArr 0.5
          q3 = fromMaybe zero $ quantile ascArr 0.75
          iqr = q3 - q1
          low = case boundIQR of
                Just b → q1 - b * iqr
                _ → fromMaybe zero $ ascArr !! 0
          high = case boundIQR of
                Just b → q3 + b * iqr
                _ → fromMaybe zero $ ascArr !! (A.length ascArr - 1)
        in
          Tuple
            (Just { low, q1, q2, q3, high })
            (A.filter (\x → x < low || x > high) ascArr)
      where
      -- @see <https://github.com/ecomfe/echarts/blob/master/extension/dataTool/quantile.js>
      quantile ∷ Array Number → Number → Maybe Number
      quantile ascArr p = do
        let
          -- ascArr should be asc sorted
          -- 0 <= p <= 1
          h = (toNumber (A.length ascArr - 1)) * p + 1.0
          h' = floor h
          v = ascArr !! (h' - 1)
          v' = ascArr !! h'
          e = h - toNumber h'
        if e > 0.0
          then case v × v' of
            Just jv × Just jv' → Just $ jv + e * (jv' - jv)
            _ → Nothing
          else v

buildBoxplot
  ∷ Map JCursor Ax.Axis
  → ChartConfiguration
  → DSL OptionI
buildBoxplot axes conf = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
  E.grids
    $ traverse_ E.grid grids
  E.titles
    $ traverse_ E.title titles
  E.legend do
    E.items $ map ET.strItem serieNames
    E.topBottom
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
  E.xAxes
    $ for_ (A.range 0 $ numDup - 1) \ind →
      E.addXAxis do
        E.gridIndex ind
        E.axisType $ ET.Category
        E.axisLabel do
          when (nRow > 3 ∧ nRow <= 6) $ E.margin 6
          when (nRow > 6) $ E.margin 4
          E.textStyle do
            E.fontFamily "Ubuntu, sans"
            when (nRow > 3 ∧ nRow <= 6) $ E.fontSize 9
            when (nRow > 6) $ E.fontSize 6
        E.axisLine $ E.lineStyle do
          E.color $ C.rgba 184 184 184 0.8
          E.width 1
        E.splitLine $ E.lineStyle do
          E.color $ C.rgba 204 204 204 0.2
          E.width 1
        E.splitArea E.hidden
        E.items $ map ET.strItem xAxisLabels
  E.yAxes
    $ for_ (A.range 0 $ numDup - 1) \ind →
      E.addYAxis do
        E.gridIndex ind
        E.axisType $ ET.Value
        E.axisLabel do
          when (nRow > 3 ∧ nRow <= 6) $ E.margin 6
          when (nRow > 6) $ E.margin 4
          E.textStyle do
            E.fontFamily "Ubuntu, sans"
            when (nRow > 3 ∧ nRow <= 6) $ E.fontSize 9
            when (nRow > 6) $ E.fontSize 6
        E.axisLine $ E.lineStyle do
          E.color $ C.rgba 184 184 184 0.8
          E.width 1
        E.splitLine $ E.lineStyle do
          E.color $ C.rgba 204 204 204 0.2
          E.width 1
        E.splitArea E.hidden
  E.series do
    let
      d = A.concat
        $ map (\x → map (\y → Tuple (fst x) y) $ snd x)
        $ A.zip (A.range 0 numDup) $ map snd (snd boxplotData)
    traverse_ (E.boxPlot ∘ boxplotSerie) d
    traverse_ (E.scatter ∘ scatterSerie) d
  where
  boxplotData ∷ BoxplotData
  boxplotData = buildBoxplotData (buildChartAxes axes conf) dimType

  dimType ∷ ET.AxisType
  dimType =
    case conf.dimensions !! 0  >>= view _value >>= flip M.lookup axes of
      Just (Ax.TimeAxis _) → ET.Time
      Just (Ax.ValAxis _) → ET.Value
      _ → ET.Category

  xAxisLabels ∷ Array String
  xAxisLabels = fst boxplotData

  serieNames ∷ Array String
  serieNames =  A.nub $ map fst $ A.concat $ map snd $ snd boxplotData

  dupNames ∷ Array String
  dupNames = map fst $ snd boxplotData

  numDup ∷ Int
  numDup = A.length dupNames

  -- max number of plot in one row: 4
  maxCol ∷ Int
  maxCol = 4
  nRow ∷ Int
  nRow = ((numDup - 1) / maxCol + 1)
  nCol ∷ Int
  nCol = if numDup <= maxCol then numDup else maxCol
  rowHight ∷ Number
  rowHight = (100.0 - legendSpace) / (toNumber nRow)
  colWidth ∷ Number
  colWidth = 100.0 / (toNumber nCol)
  legendSpace ∷ Number
  legendSpace = 5.0

  grids ∷ Array (DSL ETP.GridI)
  grids = map mkGrid (A.range 0 $ numDup - 1)
    where
    mkGrid ∷ Int → DSL ETP.GridI
    mkGrid i = do
      let
        row = toNumber (i / nCol)
        col = (toNumber i) % (toNumber nCol)
        spaceCoeff = 0.75
      E.heightPixelOrPercent
        $ ET.Percent
        $ rowHight * spaceCoeff
      E.widthPixelOrPercent
        $ ET.Percent
        $ colWidth * spaceCoeff
      E.left
        $ ET.Percent
        $ 100.0 * (2.0 * col + 1.0) / (toNumber (nCol * 2))
          - (colWidth * spaceCoeff) / 2.0
      E.top
        $ ET.Percent
        $ (100.0 - legendSpace) * (2.0 * row + 1.0) / (toNumber (nRow * 2))
          - (rowHight * spaceCoeff) / 2.0

  titles ∷ Array (DSL ETP.TitleI)
  titles = map mkTitle (A.range 0 $ numDup - 1)
    where
    mkTitle ∷ Int → DSL ETP.TitleI
    mkTitle i = do
      let
        row = toNumber (i / nCol)
        col = (toNumber i) % (toNumber nCol)
      E.text $ fromMaybe "" $ dupNames !! i
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
        when (nRow <= 4) $ E.fontSize 12
        when (nRow > 4 ∧ nRow <= 6) $ E.fontSize 11
        when (nRow > 6) $ E.fontSize 10
      E.left
        $ ET.Percent
        $ 100.0 * (2.0 * col + 1.0) / (toNumber (nCol * 2))
          -- adjust the position to the center
          - 0.3
      E.top
        $ ET.Percent
        $ (100.0 - legendSpace) * (2.0 * row + 1.0) / (toNumber (nRow * 2))
          - rowHight / 2.0
      E.textCenter
      E.textMiddle

  boxplotSerie
    ∷ Int × (String × Array BoxDataAndOutliers)
    → DSL ETP.BoxPlotI
  boxplotSerie (ind × (s × a)) = do
    when (s ≠ "") $ E.name s
    E.xAxisIndex ind
    E.yAxisIndex ind
    E.itemStyle $ E.normalItemStyle do
      E.borderWidth 2
      when (s ≠ "")
        $ E.borderColor
        $ fromMaybe (C.rgba 0 0 0 0.5)
        $ colors !! (fromMaybe 0 $ A.elemIndex s serieNames)
    E.tooltip do
      E.formatterItemArrayValue
        \param → param.name <> "<br/>"
                  <> "Upper: " <> show (fromMaybe zero $ param.value !! 4) <> "<br/>"
                  <> "Q3: " <> show (fromMaybe zero $ param.value !! 3) <> "<br/>"
                  <> "Median: " <> show (fromMaybe zero $ param.value !! 2) <> "<br/>"
                  <> "Q2: " <> show (fromMaybe zero $ param.value !! 1)<> "<br/>"
                  <> "Lower: " <> show (fromMaybe zero $ param.value !! 0)
    E.items $ map (ET.numArrItem <<< formArray <<< fst) a
      where
      formArray ∷ Maybe BoxData → Array Number
      formArray a = case a of
        Just boxData → [boxData.low, boxData.q1, boxData.q2, boxData.q3, boxData.high]
        _ → []

  scatterSerie
    ∷ Int × (String × Array BoxDataAndOutliers)
    → DSL ETP.ScatterI
  scatterSerie (ind × (s × a)) = do
    when (s ≠ "") $ E.name s
    E.xAxisIndex ind
    E.yAxisIndex ind
    when (s == "") $ E.symbolSize 5
    -- Echarts 3 cannot properly display outlier dots when there are
    -- multiple series: all dots locate along with the same centeral
    -- line of the panel, rather than the ones of respective boxes.
    -- Temporarily hide the outlier dots when there are multiple series.
    -- This setting should be removed after Echarts 3 fixes the problem.
    -- An issue was opened to Echarts 3:
    -- <https://github.com/ecomfe/echarts/issues/3944>
    when (s ≠ "") $ E.symbolSize 0
    E.itemStyle $ E.normalItemStyle do
      when (s ≠ "")
        $ E.color
        $ fromMaybe (C.rgba 0 0 0 0.5)
        $ colors !! (fromMaybe 0 $ A.elemIndex s serieNames)
    E.tooltip do
      E.formatterItemArrayValue
        \param → param.name <> "<br/>"
                  <> show (fromMaybe zero $ param.value !! 1)
    E.items do
      let outliers = map snd a
      map ET.numArrItem
        $ A.concat
        $ map (\x → map (\y → [toNumber $ fst x, y]) $ snd x)
        $ A.zip (A.range 0 $ A.length outliers) outliers
