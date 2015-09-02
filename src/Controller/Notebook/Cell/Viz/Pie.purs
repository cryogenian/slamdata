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

module Controller.Notebook.Cell.Viz.Pie where

import Prelude

import Controller.Notebook.Common (I())
import Model.Notebook.Cell (Cell())
import Model.Notebook.Cell.Viz
import Data.Selection

import ECharts.Axis
import ECharts.Chart
import ECharts.Common
import ECharts.Coords
import ECharts.Formatter
import ECharts.Grid
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Legend
import ECharts.Options
import ECharts.Series
import ECharts.Style.Item
import ECharts.Toolbox
import ECharts.Tooltip

import Data.Int (toNumber, fromNumber)
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple (fst, snd, Tuple(..))
import Data.Array (range, filter, zipWith, replicate, length, (!!), reverse, nub, concat, groupBy, catMaybes, (:))
import Data.Map (lookup, Map(), alter, values, empty, keys, toList, fromList)
import qualified Model.Notebook.ECharts as Me
import Optic.Core
import Data.Foldable (foldl)
import Data.Argonaut.JCursor (JCursor())
import Controller.Notebook.Cell.Viz.Key
import Controller.Notebook.Cell.Viz.Bar (extractClean, AggregatedAccum())
import Data.Bifunctor (lmap, bimap)
import Data.String (split)
import Math (floor)
import Data.String (split)
import qualified Data.List as L

rowLength :: Int
rowLength = 4


mkSeries :: AggregatedAccum -> Array Series
mkSeries acc =
  concat (zipWith (rows $ length groupped) (range 0 $ length groupped) groupped)
  where
  rows :: Int -> Int -> Array PieSeriesRec -> Array Series
  rows count ix lst =
    zipWith (donut count ix $ length lst) (range 0 $ length lst) lst

  donut :: Int -> Int -> Int -> Int -> PieSeriesRec -> Series
  donut rowCount rowIx donutCount donutIx r  =
    case mkRadius rowCount rowIx of
      Tuple maxR center ->
        PieSeries {
          common: universalSeriesDefault {
             itemStyle = Just $ ItemStyle {
                emphasis: Nothing
                , normal: Just $ IStyle $ istyleDefault {
                  label = Just $ ItemLabel $ itemLabelDefault {show = Just false}
                  , labelLine = Just $ ItemLabelLine $ itemLabelLineDefault {show = Just false}
                  }
                }
             }
          , pieSeries: r { radius = radius maxR donutCount donutIx
                         , center = center
                         , startAngle = Just $ toNumber $ (45 * donutIx) `mod` 360
                         }
          }
  radius :: Number -> Int -> Int -> Maybe Radius
  radius max count ix =
    if count == 1
    then Just $ R (Percent max)
    else radius' max (toNumber count) (toNumber ix)

  radius' :: Number -> Number -> Number -> Maybe Radius
  radius' max count ix =
    let step = max / (count + 1.0)
        record = {inner: Percent (step * (ix + 1.0))
                 , outer: Percent (step * (ix + 2.0))}
    in Just $ Rs record

  mkRadius :: Int -> Int -> Tuple Number (Maybe Center)
  mkRadius count ix =
    let countNum = toNumber count
        ixNum = toNumber ix
    in if count <= rowLength
       then mkRadius' countNum ixNum
       else mkRadius'' countNum ixNum

  mkRadius' :: Number -> Number -> Tuple Number (Maybe Center)
  mkRadius' count ix =
    let r = 85.0 / count
        step = 100.0 / count
        modulus = maybe 0.0 toNumber $ mod <$> fromNumber ix <*> fromNumber count
        x = 55.0 + (modulus + 0.5 - count/2.0) * step
        y = 50.0
        c = Just $ Tuple (Percent x) (Percent y)
    in Tuple r c

  mkRadius'' :: Number -> Number -> Tuple Number (Maybe Center)
  mkRadius'' count ix =
    let l = toNumber rowLength
        r = 85.0 / l
        step = 100.0 / l
        modulus = maybe 0.0 toNumber $ mod <$> fromNumber ix <*> fromNumber l
        x = 55.0 + (modulus - l/2.0 + 0.5) * step
        y = 1.2 * floor (ix / l) * r + r
        c = Just $ Tuple (Percent x) (Percent y)
    in Tuple r c

  nameMap :: Array (Tuple Key Number) -> Map String (Array (Tuple String Number))
  nameMap = named' >>> named''

  named :: Array (Tuple Key Number) -> String -> Map String (Tuple String Number)
  named lst cat =
    (fromList <<< L.toList) $
    ((bimap keyName (Tuple cat)) <$>
     (filter (\(Tuple k _) -> keyCategory k == cat) lst))

  named' :: Array (Tuple Key Number) -> Array (Map String (Tuple String Number))
  named' lst = named lst <$> catVals

  catVals :: Array String
  catVals = nub $ keyCategory <$> ks

  ks :: Array Key
  ks = L.fromList $ keys acc

  named'' :: Array (Map String (Tuple String Number)) ->
             Map String (Array (Tuple String Number))
  named'' m =
    reverse <$> (foldl foldFn empty (L.fromList <<< toList <$> m))

  foldFn :: Map String (Array (Tuple String Number)) ->
            Array (Tuple String (Tuple String Number)) ->
            Map String (Array (Tuple String Number))
  foldFn m tpls =
    foldl (\m (Tuple k n) -> alter (alterNamed n) k m) m tpls

  alterNamed :: Tuple String Number -> Maybe (Array (Tuple String Number)) ->
                Maybe (Array (Tuple String Number))
  alterNamed n ns =
    Just $ (n:(fromMaybe [] ns))

  group :: Map String (Array (Tuple String Number))
  group = nameMap $ L.fromList $ toList acc

  dat :: String -> Tuple String Number -> ItemData
  dat str (Tuple s n) = (Dat $ (dataDefault $ Simple n) {name = Just $ s <>
                                                                (if str == ""
                                                                then ""
                                                                else ":" <> str)})

  serie :: Tuple String (Array (Tuple String Number)) ->
           Tuple String PieSeriesRec
  serie (Tuple k tpls) =
    Tuple k (pieSeriesDefault { "data" = Just $ (dat k) <$> tpls})

  series :: Array (Tuple String PieSeriesRec)
  series = serie <$> (L.fromList $ toList group)

  groupped :: Array (Array (PieSeriesRec))
  groupped =
    (snd <$>) <$>
    (groupBy (\a b -> (split ":" (fst a) !! 1) == (split ":" (fst b) !! 1)) series)


mkPie :: forall e. VizRec -> _ -> Option
mkPie r conf =
  Option $ optionDefault { tooltip = Just $ Tooltip $
                                     tooltipDefault {trigger = Just TriggerItem}
                         , series = Just $ Just <$> series
                         , legend = Just $ mkLegend series
                         }
  where
  mkLegend :: Array Series -> Legend
  mkLegend ss =
    Legend legendDefault { "data" = Just $ legendItemDefault <$> (extractNames series)
                         , orient = Just Vertical
                         , x = Just XLeft}

  extractNames :: Array Series -> Array String
  extractNames ss = nub $ catMaybes $ concat $ (extractName <$> ss)

  extractName :: Series -> Array (Maybe String)
  extractName (PieSeries r) = extractOneDatum <$> (fromMaybe [] r.pieSeries."data")
  extractname _ = []

  extractOneDatum :: ItemData -> Maybe String
  extractOneDatum (Dat r) = r.name
  extractOneDatum _ = Nothing


  series = mkSeries extracted
  extracted = extractClean r conf

