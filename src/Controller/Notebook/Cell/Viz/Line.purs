module Controller.Notebook.Cell.Viz.Line where

import Data.Maybe (Maybe(..))
import Controller.Notebook.Common (I(), update)
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
import Data.Tuple
import Data.Array (filter, zipWith, head, reverse, sort, length, replicate, nub, concat)
import Data.Map (lookup)
import qualified Model.Notebook.ECharts as Me
import Optic.Core
import Data.Foldable (foldl)
import Data.Map (Map(), keys, lookup, alter, toList, empty, fromList)
import Data.Bifunctor (bimap, lmap)
import Controller.Notebook.Cell.Viz.Key

type Accum = Map Key (Tuple [Number] [Number])
type AggregatedAccum = Map Key (Tuple Number Number)

simpleData = Value <<< Simple

extractData :: VizRec -> LineConfiguration -> Accum
extractData r conf =
  extractData' dims sers1 sers2 vals1 vals2 empty
  where
  dims :: [Maybe String]
  dims =
    (>>= Me.catFromSemanthic) <$>
    (maybe [ ] Me.runAxis ((conf ^._dims.._selection) >>= (flip lookup (r ^._all))))

  vals1 :: [Maybe Number]
  vals1 =
    (>>= Me.valFromSemanthic) <$> 
    (maybe [ ] Me.runAxis ((conf ^._firstMeasures.._selection) >>= (flip lookup (r ^._all))))

  vals2 :: [Maybe Number]
  vals2 =
    (>>= Me.valFromSemanthic) <$>
    (maybe nothings Me.runAxis ((conf ^._secondMeasures.._selection) >>= (flip lookup (r ^._all))))

  maxLen :: Number
  maxLen =
    fromMaybe 0 $ head $ reverse (sort [length vals1, length dims])

  nothings :: forall a. [Maybe a]
  nothings = replicate maxLen Nothing
  
  sers1 :: [Maybe String]
  sers1 =
    (>>= Me.catFromSemanthic) <$>
    (maybe nothings Me.runAxis ((conf ^._firstSeries.._selection) >>= (flip lookup (r ^._all))))

  sers2 :: [Maybe String]
  sers2 =
    (>>= Me.catFromSemanthic) <$>
    (maybe nothings Me.runAxis ((conf ^._secondSeries.._selection) >>= (flip lookup (r ^._all))))
  
extractData' :: [Maybe String] -> [Maybe String] -> [Maybe String] ->
                [Maybe Number] -> [Maybe Number] -> Accum -> Accum
extractData' [] _ _ _ _ acc = acc
extractData' _ [] _ _ _ acc = acc
extractData' _ _ [] _ _ acc = acc
extractData' _ _ _ [] _ acc = acc
extractData' _ _ _ _ [] acc = acc
extractData' (mbd:ds) (mbs1:sers1) (mbs2:sers2) (mbv1:vals1) (mbv2:vals2) acc =
  extractData' ds sers1 sers2 vals1 vals2 $ fromMaybe acc do
    d <- mbd
    let v1 = fromMaybe 0 mbv1
        v2 = fromMaybe 0 mbv2
        key = mkKey d mbs1 mbs2 
    pure (alter (alter' $ Tuple v1 v2) key acc)

alter' :: Tuple Number Number -> Maybe (Tuple [Number] [Number]) ->
          Maybe (Tuple [Number] [Number])
alter' (Tuple v1 v2) val =
  case fromMaybe (Tuple [] []) val of
    Tuple v1s v2s ->
      Just $ Tuple (v1:v1s) (v2:v2s)

aggregate :: Accum -> LineConfiguration -> AggregatedAccum
aggregate acc conf =
  (bimap firstAgg secondAgg) <$> acc
  where
  firstAgg = runAggregation (conf ^._firstAggregation)
  secondAgg = runAggregation (conf ^._secondAggregation)


extractClean :: VizRec -> LineConfiguration -> AggregatedAccum
extractClean r conf =
  aggregate (extractData r conf) conf

getXAxisType :: VizRec -> LineConfiguration -> AxisType
getXAxisType r conf =
  case (conf ^._dims.._selection) >>= (flip lookup (r ^._all)) of
    Just (Me.TimeAxis _) -> TimeAxis
    _ -> CategoryAxis

mkSeries :: Boolean -> AxisType -> AggregatedAccum -> Tuple Axises [Series]
mkSeries needTwoAxis ty acc =
  Tuple xAxis series
  where
  ks :: [Key]
  ks = keys acc

  series :: [Series]
  series =
    case group of
      Tuple firsts seconds ->
        (firstSerie <$> toList firsts) <>
        (if needTwoAxis
         then secondSerie <$> toList seconds
         else [])
  
  catVals :: [String]
  catVals = nub $ keyCategory <$> ks

  xAxis = OneAxis $ Axis 
          axisDefault { "type" = Just ty
                      , "data" = Just $ CommonAxisData <$> catVals
                      }

  serie :: Number -> Tuple String [Number] -> Series
  serie ix (Tuple name nums) =
    LineSeries { common: if name == ""
                         then universalSeriesDefault
                         else universalSeriesDefault { "name" = Just name }
               , lineSeries: lineSeriesDefault { "data" = Just $ simpleData <$> (nums)
                                               , yAxisIndex = Just ix
                                               }
               }
  firstSerie :: Tuple String [Number] -> Series
  firstSerie = serie 0

  secondSerie :: Tuple String [Number] -> Series
  secondSerie = serie 1

  group :: Tuple (Map String [Number]) (Map String [Number])
  group =
    bimap nameMap nameMap $
    splitSeries $ toList acc

  splitSeries :: [Tuple Key (Tuple Number Number)] ->
                 Tuple [Tuple Key Number] [Tuple Key Number]
  splitSeries src  =
    foldl (\(Tuple firsts seconds) (Tuple k (Tuple f s)) ->
            Tuple ((Tuple k f):firsts) ((Tuple k s):seconds))
    (Tuple [] []) src

  nameMap :: [Tuple Key Number] -> Map String [Number]
  nameMap = named'' <<< filled <<< named'

  named :: [Tuple Key Number] -> String -> Map String Number
  named lst cat =
    fromList 
    ((\x -> lmap keyName x) <$> 
     (filter (\(Tuple k _) -> keyCategory k == cat) lst))

  named' :: [Tuple Key Number] -> [Map String Number]
  named' lst = named lst <$> catVals

  namedKeys :: [Map String Number] -> [String]
  namedKeys ms = nub $ concat (keys <$> ms)

  filled :: [Map String Number] -> [Map String Number]
  filled ms =
    let ks = namedKeys ms in
    (\m -> foldl fill m ks) <$> ms

  fill :: Map String Number -> String ->  Map String Number
  fill m key =
    alter (\k -> case k of
              Nothing -> Just 0
              a -> a) key m

  named'' :: [Map String Number] -> Map String [Number] 
  named'' m =
    reverse <$> (foldl foldFn empty (toList <$> m))

  foldFn :: Map String [Number] -> [Tuple String Number] -> Map String [Number]
  foldFn m tpls =
    foldl (\m (Tuple k n) -> alter (alterNamed n) k m) m tpls

  alterNamed :: Number -> Maybe [Number] -> Maybe [Number]
  alterNamed n ns =
    Just $ (n:(fromMaybe [] ns))


needTwoAxises :: VizRec -> LineConfiguration -> Boolean
needTwoAxises r conf =
  isJust ((conf ^._secondMeasures.._selection) >>= (flip lookup (r ^._all)))

mkLine :: VizRec -> LineConfiguration -> Option
mkLine r conf =
  case tpls of
    Tuple xAxis series ->
      Option optionDefault { series = Just $ Just <$> series
                           , xAxis = Just xAxis
                           , yAxis = Just yAxis
                           , tooltip = Just $ Tooltip $
                                       tooltipDefault {trigger = Just TriggerItem}
                           }
  where
  xAxisType :: AxisType
  xAxisType = getXAxisType r conf

  extracted :: AggregatedAccum
  extracted = extractClean r conf 

  tpls :: Tuple Axises [Series]
  tpls = mkSeries (needTwoAxises r conf) xAxisType extracted

  yAxis' :: Axis
  yAxis' = Axis axisDefault { "type" = Just ValueAxis}

  yAxis :: Axises
  yAxis =
    if needTwoAxises r conf
    then TwoAxises yAxis' yAxis'
    else OneAxis yAxis'
