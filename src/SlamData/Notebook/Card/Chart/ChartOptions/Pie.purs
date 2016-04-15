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

module SlamData.Notebook.Card.Chart.ChartOptions.Pie where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array ((!!), (:))
import Data.Array as A
import Data.Function (on)
import Data.Int (toNumber, fromNumber)
import Data.List as L
import Data.Map as M
import Data.String (split)

import ECharts as EC

import Math (floor)

import SlamData.Notebook.Card.Chart.Axis as Ax
import SlamData.Notebook.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Notebook.Card.Chart.ChartOptions.Common (Key, PieBarData, keyCategory, keyName, colors, buildChartAxises, pieBarData)

buildPie :: M.Map JCursor Ax.Axis -> ChartConfiguration -> EC.Option
buildPie axises conf =
  EC.Option $ EC.optionDefault
    { tooltip = tooltip
    , series = Just $ map Just series
    , legend = Just legend
    , color = Just colors
    }
  where
  tooltip :: Maybe EC.Tooltip
  tooltip = Just $ EC.Tooltip $ EC.tooltipDefault { trigger = Just EC.TriggerItem }

  extractedData :: PieBarData
  extractedData = pieBarData $ buildChartAxises axises conf

  series :: Array EC.Series
  series = mkSeries extractedData

  legend :: EC.Legend
  legend =
    EC.Legend EC.legendDefault
      { "data" = Just $ map EC.legendItemDefault $ extractNames series
      , orient = Just EC.Vertical
      , x = Just EC.XLeft
      }

  extractNames :: Array EC.Series -> Array String
  extractNames ss = A.nub $ A.catMaybes $ A.concatMap extractName ss

  extractName :: EC.Series -> Array (Maybe String)
  extractName (EC.PieSeries r) = map extractOneDatum $ fromMaybe [] r.pieSeries."data"
  extractName _ = []

  extractOneDatum :: EC.ItemData -> Maybe String
  extractOneDatum (EC.Dat r) = r.name
  extractOneDatum _ = Nothing

mkSeries :: PieBarData -> Array EC.Series
mkSeries pbData =
  A.concat (A.zipWith (rows $ A.length groupped) (A.range 0 $ A.length groupped) groupped)
  where
  rows :: Int -> Int -> Array EC.PieSeriesRec -> Array EC.Series
  rows count ix arr =
    A.zipWith (donut count ix $ A.length arr) (A.range 0 $ A.length arr) arr

  donut :: Int -> Int -> Int -> Int -> EC.PieSeriesRec -> EC.Series
  donut rowCount rowIx donutCount donutIx r = case maxRadius rowCount rowIx of
    Tuple maxR center -> EC.PieSeries
      { common:
          EC.universalSeriesDefault
            { itemStyle = Just $ EC.ItemStyle
                { emphasis: Nothing
                , normal: Just $ EC.IStyle $ EC.istyleDefault
                    { label = Just $ EC.ItemLabel $ EC.itemLabelDefault
                        { show = Just false }
                    , labelLine = Just $ EC.ItemLabelLine
                        $ EC.itemLabelLineDefault { show = Just false }
                    }
                }
            }
      , pieSeries:
          r
            { radius = radius maxR donutCount donutIx
            , center = center
            , startAngle = Just $ toNumber $ (45 * donutIx) `mod` 360
            }
      }

  maxRadius :: Int -> Int -> Tuple Number (Maybe EC.Center)
  maxRadius count ix =
    let countNum = toNumber count
        ixNum = toNumber ix
    in if count <= rowLength
       then maxRadiusOneRow countNum ixNum
       else maxRadiusManyRows countNum ixNum

  maxRadiusOneRow :: Number -> Number -> Tuple Number (Maybe EC.Center)
  maxRadiusOneRow count ix =
    let r = 85.0 / count
        step = 100.0 / count
        modulus = maybe 0.0 toNumber $ mod <$> fromNumber ix <*> fromNumber count
        x = 55.0 + (modulus + 0.5 - count/2.0) * step
        y = 50.0
        c = Just $ Tuple (EC.Percent x) (EC.Percent y)
    in Tuple r c

  maxRadiusManyRows :: Number -> Number -> Tuple Number (Maybe EC.Center)
  maxRadiusManyRows count ix =
    let l = toNumber rowLength
        r = 85.0 / l
        step = 100.0 / l
        modulus = maybe 0.0 toNumber $ mod <$> fromNumber ix <*> fromNumber l
        x = 55.0 + (modulus - l/2.0 + 0.5) * step
        y = 1.2 * floor (ix/l) * r + r
        c = Just $ Tuple (EC.Percent x) (EC.Percent y)
    in Tuple r c

  radius :: Number -> Int -> Int -> Maybe EC.Radius
  radius max count ix =
    if count == 1
    then Just $ EC.R (EC.Percent max)
    else donutRadius max (toNumber count) (toNumber ix)

  donutRadius :: Number -> Number -> Number -> Maybe EC.Radius
  donutRadius max count ix =
    let step = max / (count + 1.0)
        record = { inner: EC.Percent (step * (ix + 1.0))
                 , outer: EC.Percent (step * (ix + 2.0))
                 }
    in Just $ EC.Rs record

  groupped :: Array (Array EC.PieSeriesRec)
  groupped = map (map snd) $ A.groupBy (on eq ((_ !! 1) <<< split ":" <<< fst)) series

  series :: Array (Tuple String EC.PieSeriesRec)
  series = map serie $ L.fromList $ M.toList group

  group :: M.Map String (Array (Tuple String Number))
  group = nameMap $ L.fromList $ M.toList pbData

  ks :: Array Key
  ks = L.fromList $ M.keys pbData

  catVals :: Array String
  catVals = A.nub $ map keyCategory ks

  groupByCategories
    :: Array (Tuple Key Number) -> Array (M.Map String (Tuple String Number))
  groupByCategories arr = map (filterAndMarkCategory arr) catVals

  filterAndMarkCategory
    :: Array (Tuple Key Number) -> String -> M.Map String (Tuple String Number)
  filterAndMarkCategory arr cat =
      M.fromList
    $ L.toList
    $ map (bimap keyName (Tuple cat))
    $ A.filter (\(Tuple k _) -> keyCategory k == cat)
    $ arr

  mapByCategories
    :: Array (M.Map String (Tuple String Number))
    -> M.Map String (Array (Tuple String Number))
  mapByCategories arr =
    map A.reverse $ foldl foldFn M.empty (L.fromList <<< M.toList <$> arr)

  nameMap :: Array (Tuple Key Number) -> M.Map String (Array (Tuple String Number))
  nameMap = groupByCategories >>> mapByCategories

  foldFn
    :: M.Map String (Array (Tuple String Number))
    -> Array (Tuple String (Tuple String Number))
    -> M.Map String (Array (Tuple String Number))
  foldFn m tpls =
    foldl (\m (Tuple k n) -> M.alter (alterNamed n) k m) m tpls

  alterNamed
    :: Tuple String Number -> Maybe (Array (Tuple String Number))
    -> Maybe (Array (Tuple String Number))
  alterNamed n ns = Just $ n : fromMaybe [] ns

  serie
    :: Tuple String (Array (Tuple String Number))
    -> Tuple String EC.PieSeriesRec
  serie (Tuple k tpls) =
    Tuple k $ EC.pieSeriesDefault { "data" = Just $ map (dat k) $ tpls }

  dat :: String -> Tuple String Number -> EC.ItemData
  dat str (Tuple s n) =
    EC.Dat $
      (EC.dataDefault $ EC.Simple n)
        { name = Just $ s <> (if str == "" then "" else ":" <> str) }

rowLength :: Int
rowLength = 4
