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
import Data.Bifunctor (lmap, bimap)
import Data.String (split)
import Math (floor)
import Data.String (split)

rowLength :: Number
rowLength = 4

mkSeries :: AggregatedAccum -> [Series]
mkSeries acc =
  concat (zipWith (rows $ length groupped) (range 0 $ length groupped) groupped)
  where
  rows :: Number -> Number -> [PieSeriesRec] -> [Series]
  rows count ix lst =
    zipWith (donut count ix $ length lst) (range 0 $ length lst) lst 

  donut :: Number -> Number -> Number -> Number -> PieSeriesRec -> Series
  donut rowCount rowIx donutCount donutIx r  =
    case mkRadius rowCount rowIx of
      Tuple maxR center -> 
        PieSeries { common: universalSeriesDefault
                  , pieSeries: r { radius = radius maxR donutCount donutIx
                                 , center = center
                                 , startAngle = Just $ (45 * donutIx) % 360
                                 }
                  }
  radius :: Number -> Number -> Number -> Maybe Radius
  radius max count ix =
    if count == 1
    then Just $ R (Percent max)
    else radius' max count ix

  radius' :: Number -> Number -> Number -> Maybe Radius
  radius' max count ix =
    let step = max / (count + 1)
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

  nameMap :: [Tuple Key Number] -> Map String [Tuple String Number]
  nameMap = named' >>> named''

  named :: [Tuple Key Number] -> String -> Map String (Tuple String Number)
  named lst cat =
    fromList
    ((bimap keyName (Tuple cat)) <$>
     (filter (\(Tuple k _) -> keyCategory k == cat) lst))

  named' :: [Tuple Key Number] -> [Map String (Tuple String Number)]
  named' lst = named lst <$> catVals

  catVals :: [String]
  catVals = nub $ keyCategory <$> ks

  ks :: [Key]
  ks = keys acc

  named'' :: [Map String (Tuple String Number)] -> Map String [Tuple String Number]
  named'' m =
    reverse <$> (foldl foldFn empty (toList <$> m))

  foldFn :: Map String [Tuple String Number] ->
            [Tuple String (Tuple String Number)] ->
            Map String [Tuple String Number]
  foldFn m tpls =
    foldl (\m (Tuple k n) -> alter (alterNamed n) k m) m tpls

  alterNamed :: Tuple String Number -> Maybe [Tuple String Number] ->
                Maybe [Tuple String Number]
  alterNamed n ns =
    Just $ (n:(fromMaybe [] ns))

  group :: Map String [Tuple String Number]
  group = nameMap $ toList acc

  dat :: String -> Tuple String Number -> ItemData 
  dat str (Tuple s n) = (Dat $ (dataDefault $ Simple n) {name = Just $ s <>
                                                                (if str == ""
                                                                then ""
                                                                else ":" <> str)})

  serie :: Tuple String [Tuple String Number] -> Tuple String PieSeriesRec 
  serie (Tuple k tpls) =
    Tuple k (pieSeriesDefault { "data" = Just $ (dat k) <$> tpls})

  series :: [Tuple String PieSeriesRec]
  series = serie <$> (toList group)

  groupped :: [[PieSeriesRec]]
  groupped =
    (snd <$>) <$>
    (groupBy (\a b -> (split ":" (fst a) !! 1) == (split ":" (fst b) !! 1)) series)

  
mkPie :: forall e. VizRec -> _ -> Option
mkPie r conf =
  Option $ optionDefault { tooltip = Just $ Tooltip $
                                     tooltipDefault {trigger = Just TriggerItem}
                         , series = Just $ Just <$> series
                         }
  where
  series = mkSeries extracted
  extracted = extractClean r conf 
  
