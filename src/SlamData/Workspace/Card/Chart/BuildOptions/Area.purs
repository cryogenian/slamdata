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

module SlamData.Workspace.Card.Chart.BuildOptions.Area where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array ((!!), cons)
import Data.Array as A
import Data.Foldable as F
import Data.Function (on)
import Data.Int as Int
import Data.Lens (view)
import Data.List (List(..), replicate, length, zip, range)
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.String as Str
import Data.Maybe.Unsafe (fromJust)

import ECharts as EC

import SlamData.Form.Select (_value)
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (Key, ChartAxises, commonNameMap, keyCategory, colors, mixAxisLabelAngleAndFontSize, buildChartAxises, mkKey, getShadeColor, toRGBAString)

import Math as Math

import Utils (stringToNumber)
import Utils.DOM (getTextWidthPure)

type LabeledPointPairs = M.Map Key (Tuple (Array Number) (Array Number))
type LineData = L.List (Tuple Key (Tuple Number Number))

lineData ∷ ChartAxises → LineData
lineData axises =
  let
    lr =
      lineRawData
        dimensions
        firstSeries
        secondSeries
        firstValues
        secondValues
        M.empty
  in
    aggregatePairs firstAgg secondAgg lr
  where
  firstAgg ∷ Maybe Aggregation
  firstAgg = fromMaybe (Just Sum) $ join (axises.aggregations !! 0)

  secondAgg ∷ Maybe Aggregation
  secondAgg = fromMaybe (Just Sum) $ join (axises.aggregations !! 1)

  dimensions ∷ List (Maybe String)
  dimensions = fromMaybe Nil $ axises.dimensions !! 0

  firstValues ∷ List (Maybe Number)
  firstValues = fromMaybe Nil $ axises.measures !! 0

  firstSeries ∷ List (Maybe String)
  firstSeries = fromMaybe nothings $ axises.series !! 0

  secondSeries ∷ List (Maybe String)
  secondSeries = fromMaybe nothings $ axises.series !! 1

  secondValues ∷ List (Maybe Number)
  secondValues = fromMaybe nothings $ axises.measures !! 1

  nothings ∷ ∀ a. List (Maybe a)
  nothings = flip replicate Nothing $ maxLen firstValues dimensions

  maxLen ∷ ∀ a b. List a → List b → Int
  maxLen lstA lstB =
    let lA = length lstA
        lB = length lstB
    in if lA > lB then lA else lB


lineRawData
  ∷ List (Maybe String)
   → List (Maybe String)
   → List (Maybe String)
   → List (Maybe Number)
   → List (Maybe Number)
   → LabeledPointPairs
   → LabeledPointPairs
lineRawData Nil _ _ _ _ acc = acc
lineRawData _ Nil _ _ _ acc = acc
lineRawData _ _ Nil _ _ acc = acc
lineRawData _ _ _ Nil _ acc = acc
lineRawData _ _ _ _ Nil acc = acc
lineRawData (Cons Nothing _) _ _ _ _ acc = acc
lineRawData
  (Cons (Just dimension) dims)
  (Cons mbFirstSerie firstSeries)
  (Cons mbSecondSerie secondSeries)
  (Cons mbFirstValue firstValues)
  (Cons mbSecondValue secondValues)
  acc =
    lineRawData dims firstSeries secondSeries firstValues secondValues
    $ M.alter (alterFn $ Tuple firstVal secondVal) key acc
  where
  firstVal ∷ Number
  firstVal = fromMaybe zero mbFirstValue

  secondVal ∷ Number
  secondVal = fromMaybe zero mbSecondValue

  key ∷ Key
  key = mkKey dimension mbFirstSerie mbSecondSerie

  alterFn
    ∷ Tuple Number Number → Maybe (Tuple (Array Number) (Array Number))
    → Maybe (Tuple (Array Number) (Array Number))
  alterFn (Tuple v1 v2) acc =
    case fromMaybe (Tuple [] []) acc of
      Tuple v1s v2s → pure $ Tuple (cons v1 v1s) (cons v2 v2s)


-- 'Nothing' is not suitable for aggreation of Pie and Bar Chart.
-- To avoid 'Nothing', control the options in aggreation selector.
-- In case that aggreation is 'Nothing', coerce it to be replaced by 'Just Sum'.
aggregatePairs ∷ Maybe Aggregation → Maybe Aggregation → LabeledPointPairs → LineData
aggregatePairs fAgg sAgg lp = 
  M.toList $ map 
    ( bimap 
        (runAggregation (if isNothing fAgg then Sum else (fromJust fAgg))) 
        (runAggregation (if isNothing sAgg then Sum else (fromJust sAgg)))  
    ) lp

buildArea
  ∷ M.Map JCursor Ax.Axis
   → Int
   → Int
   → Boolean
   → Boolean
   → ChartConfiguration
   → EC.Option
buildArea axises angle size stacked smooth conf = case preSeries of
  xAxis × series × longestCat →
    EC.Option EC.optionDefault
      { series = Just $ map Just series
      , xAxis =
          Just $ EC.OneAxis $ EC.Axis
          $ mixAxisLabelAngleAndFontSize angle size xAxis
      , yAxis = Just yAxis
      , tooltip = Just tooltip
      , legend = Just $ mkLegend series
      , color = Just colors
      , grid = Just $ EC.Grid EC.gridDefault
          { y2 = Just $ EC.Pixel $ labelHeight $ fromMaybe "" longestCat
          }
      }
  where
  labelHeight ∷ String → Number
  labelHeight longestCat =
    let
      width = getTextWidthPure longestCat $ "normal " <> show size <> "px Ubuntu"
    in
      add 24.0
        $ Math.max (Int.toNumber size + 2.0)
        $ Math.abs
        $ width
        * Math.sin (Int.toNumber angle / 180.0 * Math.pi)

  mkLegend ∷ Array EC.Series → EC.Legend
  mkLegend ss =
    EC.Legend EC.legendDefault
      { "data" = Just $ map EC.legendItemDefault $ extractNames ss
      , textStyle = Just $ EC.TextStyle EC.textStyleDefault
          { fontFamily = Just "Ubuntu" }
      }

  tooltip ∷ EC.Tooltip
  tooltip = EC.Tooltip $ EC.tooltipDefault 
    { trigger = Just EC.TriggerAxis
    , textStyle = Just $ EC.TextStyle EC.textStyleDefault 
        { fontFamily = Just "Ubuntu"
        , fontSize = Just 12.0 
        }
    , axisPointer = Just $ EC.TooltipAxisPointer EC.tooltipAxisPointerDefault 
        { "type" = Just $ EC.LinePointer
        , lineStyle = Just $ EC.LineStyle EC.lineStyleDefault 
            { color = Just "rgba(170,170,170,0.8)"
            , width = Just 1.0
            , "type" = Just $ EC.Solid
            }
        }
    }

  extractNames ∷ Array EC.Series → Array String
  extractNames ss = A.nub $ A.catMaybes $ map extractName ss

  extractName ∷ EC.Series → Maybe String
  extractName (EC.LineSeries r) = r.common.name
  extractName _ = Nothing

  xAxisConfig ∷ Tuple EC.AxisType (Maybe EC.Interval)
  xAxisConfig = getXAxisConfig axises conf

  extracted ∷ LineData
  extracted =
    L.sortBy (mkSortFn `on` (fst ⋙ keyCategory))
      $ lineData $ buildChartAxises axises conf

  mkSortFn
    ∷ String → String → Ordering
  mkSortFn =
    case (conf.dimensions !! 0) >>= view _value >>= flip M.lookup axises of
      Just (Ax.ValAxis _) → compare `on` stringToNumber
      _ → compare

  yAxis ∷ EC.Axises
  yAxis =
    if needTwoAxises axises conf
    then EC.TwoAxises yAxis' yAxis'
    else EC.OneAxis yAxis'

  yAxis' ∷ EC.Axis
  yAxis' =
    EC.Axis EC.axisDefault
      { "type" = Just EC.ValueAxis
      , axisLabel = Just $ EC.AxisLabel EC.axisLabelDefault
        { textStyle = Just $ EC.TextStyle EC.textStyleDefault
          { fontFamily = Just "Ubuntu"
          , fontSize = Just $ Int.toNumber size
          }
        }
      , axisLine = Just $ EC.AxisLine EC.axisLineDefault 
        { lineStyle = Just $ EC.AxisLineStyle EC.axisLineStyleDefault 
            { color = Just "rgba(184,184,184,0.8)"
            , width = Just 0.5
            }
        }
      , splitLine = Just $ EC.AxisSplitLine EC.axisSplitLineDefault 
        { lineStyle = Just $ EC.LineStyle EC.lineStyleDefault 
          { color = Just "rgba(204,204,204,0.2)"
          , width = Just 1.0
          }
         }
      }

  preSeries ∷ EC.AxisRec × (Array EC.Series) × (Maybe String)
  preSeries = mkSeries (needTwoAxises axises conf) xAxisConfig extracted stacked smooth

needTwoAxises ∷ M.Map JCursor Ax.Axis → ChartConfiguration → Boolean
needTwoAxises axises conf =
  isJust $ (conf.measures !! 1) >>= view _value >>= flip M.lookup axises

getXAxisConfig
  ∷ M.Map JCursor Ax.Axis
  → ChartConfiguration
  → Tuple EC.AxisType (Maybe EC.Interval)
getXAxisConfig axises conf =
  case (conf.dimensions !! 0) >>= view _value >>= flip M.lookup axises of
    Just (Ax.TimeAxis _) → Tuple EC.TimeAxis $ Just $ EC.Custom zero
    Just (Ax.ValAxis _) → Tuple EC.CategoryAxis Nothing
    _ → Tuple EC.CategoryAxis $ Just EC.Auto

mkSeries
  ∷ Boolean
   → Tuple EC.AxisType (Maybe EC.Interval)
   → LineData
   → Boolean
   → Boolean
   → EC.AxisRec × (Array EC.Series) × (Maybe String)
mkSeries needTwoAxis (Tuple ty interval_) lData stacked smooth =
  xAxis × series × longestCat
  where
  longestCat ∷ Maybe String
  longestCat =
    F.maximumBy (\a b → compare (Str.length a) (Str.length b)) catVals

  xAxis ∷ EC.AxisRec
  xAxis =
    EC.axisDefault
      { "type" = Just ty
      , "data" = Just $ map EC.CommonAxisData catVals
      , boundaryGap = Just $ EC.CatBoundaryGap false
      , axisTick = Just $ EC.AxisTick EC.axisTickDefault
        { interval = interval_
        , length = Just $ 2.0
        , lineStyle = Just $ EC.LineStyle EC.lineStyleDefault 
          { color = Just "rgba(184,184,184,0.8)"
          , width = Just 1.0
          }  
        }
      , axisLine = Just $ EC.AxisLine EC.axisLineDefault 
        { lineStyle = Just $ EC.AxisLineStyle EC.axisLineStyleDefault 
          { color = Just "rgba(184,184,184,0.8)"
          , width = Just 1.0
          }
        }
      , splitLine = Just $ EC.AxisSplitLine EC.axisSplitLineDefault 
        { lineStyle = Just $ EC.LineStyle EC.lineStyleDefault 
          { color = Just "rgba(204,204,204,0.2)"
          , width = Just 1.0
          }
        }
      }

  catVals ∷ Array String
  catVals = A.nub $ map keyCategory keysArray

  keysArray ∷ Array Key
  keysArray = F.foldMap (pure ∘ fst) lData

  series ∷ Array EC.Series
  series = case group of
    Tuple firsts seconds →
      L.fromList $
      (map firstSerie $ zip (range 0 ((length (M.toList firsts))-1)) (M.toList firsts))
      <> (if needTwoAxis
          then map secondSerie $ zip (range 0 ((length (M.toList seconds))-1)) (M.toList seconds)
          else Nil
         )

  group ∷ Tuple (Map String (Array Number)) (Map String (Array Number))
  group = bimap nameMap nameMap $ splitSeries $ L.fromList lData

  splitSeries
    ∷ Array (Tuple Key (Tuple Number Number))
    → Tuple (Array (Tuple Key Number)) (Array (Tuple Key Number))
  splitSeries src =
    foldl (\(Tuple firsts seconds) (Tuple k (Tuple f s)) →
            Tuple (A.cons (Tuple k f) firsts) (A.cons (Tuple k s) seconds))
    (Tuple [] []) src

  nameMap ∷ Array (Tuple Key Number) → Map String (Array Number)
  nameMap = commonNameMap fillEmpties catVals

  arrKeys ∷ Array (Map String Number) → Array String
  arrKeys ms = A.nub $ A.concat (L.fromList ∘ M.keys <$> ms)

  fillEmpties ∷ Array (Map String Number) → Array (Map String Number)
  fillEmpties ms =
    let ks = arrKeys ms
    in map (\m → foldl fill m ks) ms

  fill ∷ Map String Number → String → Map String Number
  fill m key = M.alter (maybe (Just 0.0) Just) key m

  firstSerie ∷ Tuple Int (Tuple String (Array Number)) → EC.Series
  firstSerie = serie 0.0

  secondSerie ∷ Tuple Int (Tuple String (Array Number)) → EC.Series
  secondSerie = serie 1.0

  serie ∷ Number → Tuple Int (Tuple String (Array Number)) → EC.Series
  serie ix (Tuple ind (Tuple name nums)) = 
    EC.LineSeries 
      { common: EC.universalSeriesDefault 
        { name = if name ≡ "" 
                 then if needTwoAxis 
                      then if ix ≡ 0.0 
                           then Just $ "Left" 
                           else Just $ "Right"
                      else Nothing
                 else if needTwoAxis 
                      then if ix ≡ 0.0
                           then Just $ "Left: " <> name 
                           else Just $ "Right: " <> name
                      else Just name
        , itemStyle = Just $ EC.ItemStyle EC.itemStyleDefault 
            { normal = Just $ EC.IStyle EC.istyleDefault 
              { color = Just $ EC.SimpleColor $
                  fromMaybe "#000000" $ colors !! 
                    ( (Int.round ix) * ((A.length colors) - 1) + 
                        (1 - 2 * (Int.round ix)) * (mod ind (A.length colors)) )            
              , lineStyle = Just $ EC.LineStyle EC.lineStyleDefault 
                  { width = Just 2.0 }
              , areaStyle = Just $ EC.AreaStyle EC.areaStyleDefault
                  { color = Just $ EC.SimpleColor $ toRGBAString $ getShadeColor
                    (fromMaybe "#000000" $ colors !! 
                      ( (Int.round ix) * ((A.length colors) - 1) + 
                        (1 - 2 * (Int.round ix)) * (mod ind (A.length colors)) )            
                    )
                    (if stacked then 1.0 else 0.5)
                  } 
              }     
            }
        }
      , lineSeries: EC.lineSeriesDefault
          { "data" = Just $ map simpleData nums
          , yAxisIndex = Just ix
          , symbol = Just $ EC.Circle
          , symbolSize = Just $ EC.Size 0.0
          , stack = if stacked then Just $ "stacked on" ++ (show ix) else Nothing
          , smooth = Just smooth
          }
      }

  simpleData ∷ Number → EC.ItemData
  simpleData n = EC.Value $ EC.Simple n
