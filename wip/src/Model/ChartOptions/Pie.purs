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

module Model.ChartOptions.Pie where

import Prelude

import Data.Argonaut (JCursor())
import Data.Array
  ( catMaybes, (!!), nub, concatMap, zipWith, length, range, concat
  , groupBy, cons, filter, reverse)
import Data.Bifunctor (bimap)
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Int (toNumber, fromNumber)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (split)
import Data.Tuple (Tuple(..), snd, fst)
import ECharts
import Math (floor)
import Model.ChartAxis as Ax
import Model.ChartConfiguration (ChartConfiguration())
import Model.ChartOptions.Common

buildPie :: M.Map JCursor Ax.Axis -> ChartConfiguration -> Option
buildPie axises conf =
  Option $ optionDefault { tooltip = tooltip
                         , series = Just $ map Just series
                         , legend = Just $ legend
                         }
  where
  tooltip :: Maybe Tooltip
  tooltip = Just $ Tooltip $ tooltipDefault { trigger = Just TriggerItem }

  extractedData :: PieBarData
  extractedData = pieBarData $ buildChartAxises axises conf

  series :: Array Series
  series = mkSeries extractedData

  legend :: Legend
  legend =
    Legend legendDefault
    { "data" = Just $ map legendItemDefault $ extractNames series
    , orient = Just Vertical
    , x = Just XLeft
    }

  extractNames :: Array Series -> Array String
  extractNames ss = nub $ catMaybes $ concatMap extractName ss

  extractName :: Series -> Array (Maybe String)
  extractName (PieSeries r) = map extractOneDatum $ fromMaybe [] r.pieSeries."data"
  extractName _ = []

  extractOneDatum :: ItemData -> Maybe String
  extractOneDatum (Dat r) = r.name
  extractOneDatum _ = Nothing

mkSeries :: PieBarData -> Array Series
mkSeries pbData =
  concat (zipWith (rows $ length groupped) (range 0 $ length groupped) groupped)
  where
  rows :: Int -> Int -> Array PieSeriesRec -> Array Series
  rows count ix arr =
    zipWith (donut count ix $ length arr) (range 0 $ length arr) arr

  donut :: Int -> Int -> Int -> Int -> PieSeriesRec -> Series
  donut rowCount rowIx donutCount donutIx r = case maxRadius rowCount rowIx of
    Tuple maxR center -> PieSeries
      $ { common: universalSeriesDefault
                    { itemStyle = Just $ ItemStyle
                            { emphasis: Nothing
                            , normal: Just $ IStyle $ istyleDefault
                              { label = Just $ ItemLabel $ itemLabelDefault
                                          { show = Just false }
                              , labelLine = Just $ ItemLabelLine
                                            $ itemLabelLineDefault
                                              { show = Just false }
                              }
                            }
              }
      , pieSeries: r { radius = radius maxR donutCount donutIx
                     , center = center
                     , startAngle = Just $ toNumber $ (45 * donutIx) `mod` 360
                     }
      }

  maxRadius :: Int -> Int -> Tuple Number (Maybe Center)
  maxRadius count ix =
    let countNum = toNumber count
        ixNum = toNumber ix
    in if count <= rowLength
       then maxRadiusOneRow countNum ixNum
       else maxRadiusManyRows countNum ixNum

  maxRadiusOneRow :: Number -> Number -> Tuple Number (Maybe Center)
  maxRadiusOneRow count ix =
    let r = 85.0 / count
        step = 100.0 / count
        modulus = maybe 0.0 toNumber $ mod <$> fromNumber ix <*> fromNumber count
        x = 55.0 + (modulus + 0.5 - count/2.0) * step
        y = 50.0
        c = Just $ Tuple (Percent x) (Percent y)
    in Tuple r c

  maxRadiusManyRows :: Number -> Number -> Tuple Number (Maybe Center)
  maxRadiusManyRows count ix =
    let l = toNumber rowLength
        r = 85.0 / l
        step = 100.0 / l
        modulus = maybe 0.0 toNumber $ mod <$> fromNumber ix <*> fromNumber l
        x = 55.0 + (modulus - l/2.0 + 0.5) * step
        y = 1.2 * floor (ix/l) * r + r
        c = Just $ Tuple (Percent x) (Percent y)
    in Tuple r c

  radius :: Number -> Int -> Int -> Maybe Radius
  radius max count ix =
    if count == 1
    then Just $ R (Percent max)
    else donutRadius max (toNumber count) (toNumber ix)

  donutRadius :: Number -> Number -> Number -> Maybe Radius
  donutRadius max count ix =
    let step = max / (count + 1.0)
        record = { inner: Percent (step * (ix + 1.0))
                 , outer: Percent (step * (ix + 2.0))
                 }
    in Just $ Rs record

  groupped :: Array (Array PieSeriesRec)
  groupped = map (map snd) $ groupBy (on eq ((!! 1) <<< split ":" <<< fst)) series

  series :: Array (Tuple String PieSeriesRec)
  series = map serie $ L.fromList $ M.toList group

  group :: M.Map String (Array (Tuple String Number))
  group = nameMap $ L.fromList $ M.toList pbData

  ks :: Array Key
  ks = L.fromList $ M.keys pbData

  catVals :: Array String
  catVals = nub $ map keyCategory ks

  groupByCategories
    :: Array (Tuple Key Number) -> Array (M.Map String (Tuple String Number))
  groupByCategories arr = map (filterAndMarkCategory arr) catVals

  filterAndMarkCategory
    :: Array (Tuple Key Number) -> String -> M.Map String (Tuple String Number)
  filterAndMarkCategory arr cat =
      M.fromList
    $ L.toList
    $ map (bimap keyName (Tuple cat))
    $ filter (\(Tuple k _) -> keyCategory k == cat)
    $ arr

  mapByCategories
    :: Array (M.Map String (Tuple String Number))
    -> M.Map String (Array (Tuple String Number))
  mapByCategories arr =
    map reverse $ foldl foldFn M.empty (L.fromList <<< M.toList <$> arr)

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
  alterNamed n ns = Just $ cons n $ fromMaybe [] ns

  serie
    :: Tuple String (Array (Tuple String Number))
    -> Tuple String PieSeriesRec
  serie (Tuple k tpls) =
    Tuple k $ pieSeriesDefault { "data" = Just $ map (dat k) $ tpls }

  dat :: String -> Tuple String Number -> ItemData
  dat str (Tuple s n) =
    Dat $ (dataDefault $ Simple n) { name = Just $ s <> (if str == ""
                                                         then ""
                                                         else ":" <> str)
                                   }

rowLength :: Int
rowLength = 4
