module Controller.Notebook.Cell.Viz.Pie where

import Controller.Notebook.Common (I())
import Model.Notebook.Cell (Cell())
import Model.Notebook.Cell.Viz 


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

import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple
import Data.Array (range, filter, zipWith, replicate, length, (!!), reverse, nub, concat, groupBy)
import Data.Map (lookup, Map(), alter, values, empty, keys, toList, fromList)
import qualified Model.Notebook.ECharts as Me
import Optic.Core
import Data.Foldable (foldl)
import Data.Argonaut.JCursor (JCursor())
import Controller.Notebook.Cell.Viz.Key
import Controller.Notebook.Cell.Viz.Bar (extractClean, AggregatedAccum())
import Data.Bifunctor (lmap)
import Data.String (split)
import Math (floor)

rowLength :: Number
rowLength = 4

mkSeries :: AggregatedAccum -> [Series]
mkSeries acc =
  concat (zipWith (rows $ length srcs) (range 0 $ length srcs) srcs)
  where
  rows :: Number -> Number -> [[Tuple Key ItemData]] -> [Series]
  rows count ix lst =
    zipWith (donut count ix $ length lst) (range 0 $ length lst) lst 

  donut :: Number -> Number -> Number -> Number -> [Tuple Key ItemData] -> Series
  donut rowCount rowIx donutCount donutIx src =
    case mkRadius rowCount rowIx of
      Tuple maxR center -> 
        PieSeries { common: universalSeriesDefault
                  , pieSeries: pieSeriesDefault { "data" = Just $ snd <$> src
                                                , center = center
                                                , radius = radius maxR donutCount donutIx 
                                                }
                  }
  radius :: Number -> Number -> Number -> Maybe Radius
  radius max count ix =
    if count == 1
    then Just $ R (Percent max)
    else radius' max count ix

  radius' :: Number -> Number -> Number -> Maybe Radius
  radius' max count ix =
    let step = max / count
        record = {inner: Percent (step * (ix + 1)), outer: Percent (step * (ix + 2))}
    in Just $ Rs record
    
  mkRadius :: Number -> Number -> Tuple Number (Maybe Center)
  mkRadius count ix =
    if count < rowLength
    then mkRadius' count ix
    else mkRadius'' count ix

  mkRadius' :: Number -> Number -> Tuple Number (Maybe Center)
  mkRadius' count ix =
    let r = 80 / count
        y = r / 2 + 10
        x = r * ix + r / 2  
        c = Just $ Tuple (Percent x) (Percent y)
    in Tuple r c

  mkRadius'' :: Number -> Number -> Tuple Number (Maybe Center)
  mkRadius'' count ix =
    let r = 80 / rowLength
        y = 1.2 * floor (ix / rowLength) * r + r
        x = r * (ix % rowLength) + r / 2
        c = Just $ Tuple (Percent x) (Percent y)
    in Tuple r c

  itemDatas :: [Tuple Key ItemData]
  itemDatas = mkItemData <$> (toList acc)

  mkItemData :: Tuple Key Number -> Tuple Key ItemData
  mkItemData (Tuple k n) =
    Tuple k 
    (Dat $ (dataDefault $ Simple n) {name = Just $ str})
    where name = keyName k
          cat = keyCategory k
          str = cat <> (if name == ""
                        then ""
                        else " " <> name)

  src :: [[Tuple Key ItemData]]
  src = groupBy (\(Tuple a _) (Tuple b _) -> keyMbSeries1 a == keyMbSeries1 b) itemDatas

  srcs :: [[[Tuple Key ItemData]]]
  srcs = (groupBy (\(Tuple a _) (Tuple b _) -> keyMbSeries2 a == keyMbSeries2 b)) <$> src

  mkSerie :: [Tuple Key ItemData] -> Series
  mkSerie lst =
    PieSeries { common: universalSeriesDefault
              , pieSeries: pieSeriesDefault { "data" = Just $ snd <$> lst }
              }

mkPie :: forall e. VizRec -> _ -> Option
mkPie r conf =
  Option $ optionDefault { tooltip = Just $ Tooltip $
                                     tooltipDefault {trigger = Just TriggerItem}
                         , series = Just $ Just <$> series
                         }
  where
  series = mkSeries extracted
  extracted = extractClean r conf 
  
