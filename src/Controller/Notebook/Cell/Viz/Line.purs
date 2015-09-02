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

module Controller.Notebook.Cell.Viz.Line where

import Prelude
import Data.Maybe (Maybe(..))
import Controller.Notebook.Common (I(), update)
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

import Data.Maybe
import Data.Tuple (Tuple(..))
import Data.Array (filter, zipWith, head, reverse, sort, length, replicate, nub, concat, catMaybes, tail, (:))
import Data.Map (lookup)
import qualified Model.Notebook.ECharts as Me
import Optic.Core
import Data.Foldable (foldl)
import Data.Map (Map(), keys, lookup, alter, toList, empty, fromList)
import Data.Bifunctor (bimap, lmap)
import Controller.Notebook.Cell.Viz.Key
import qualified Data.List as L


type Accum = Map Key (Tuple (Array Number) (Array Number))
type AggregatedAccum = Map Key (Tuple Number Number)

simpleData = Value <<< Simple

extractData :: VizRec -> _ -> Accum
extractData r conf =
  extractData' dims sers1 sers2 vals1 vals2 empty
  where
  dims :: Array (Maybe String)
  dims =
    (>>= Me.catFromSemanthic) <$>
    (maybe [ ] Me.runAxis ((conf ^._dims.._selection) >>= (flip lookup (r ^._all))))

  vals1 :: Array (Maybe Number)
  vals1 =
    (>>= Me.valFromSemanthic) <$>
    (maybe [ ] Me.runAxis ((conf ^._firstMeasures.._selection) >>= (flip lookup (r ^._all))))

  vals2 :: Array (Maybe Number)
  vals2 =
    (>>= Me.valFromSemanthic) <$>
    (maybe nothings Me.runAxis ((conf ^._secondMeasures.._selection) >>= (flip lookup (r ^._all))))

  maxLen :: Int
  maxLen =
    fromMaybe zero $ head $ reverse (sort [length vals1, length dims])

  nothings :: forall a. Array (Maybe a)
  nothings = replicate maxLen Nothing

  sers1 :: Array (Maybe String)
  sers1 =
    (>>= Me.catFromSemanthic) <$>
    (maybe nothings Me.runAxis ((conf ^._firstSeries.._selection) >>= (flip lookup (r ^._all))))

  sers2 :: Array (Maybe String)
  sers2 =
    (>>= Me.catFromSemanthic) <$>
    (maybe nothings Me.runAxis ((conf ^._secondSeries.._selection) >>= (flip lookup (r ^._all))))

extractData' :: Array (Maybe String) -> Array (Maybe String) -> Array (Maybe String) ->
                Array (Maybe Number) -> Array (Maybe Number) -> Accum -> Accum
extractData' mbds mbss1 mbss2 mbvs1 mbvs2 acc = fromMaybe acc do
  d <- head mbds >>= id
  ds <- tail mbds
  mbs1 <- head mbss1
  sers1 <- tail mbss1
  mbs2 <- head mbss2
  sers2 <- tail mbss2
  mbv1 <- head mbvs1
  vals1 <- tail mbvs1
  mbv2 <- head mbvs2
  vals2 <- tail mbvs2
  let v1 = fromMaybe 0.0 mbv1
      v2 = fromMaybe 0.0 mbv2
      key = mkKey d mbs1 mbs2
  pure $ extractData' ds sers1 sers2 vals1 vals2  $ (alter (alter' $ Tuple v1 v2) key acc)


alter' :: Tuple Number Number -> Maybe (Tuple (Array Number) (Array Number)) ->
          Maybe (Tuple (Array Number) (Array Number))
alter' (Tuple v1 v2) val =
  case fromMaybe (Tuple [] []) val of
    Tuple v1s v2s ->
      Just $ Tuple (v1:v1s) (v2:v2s)

aggregate :: Accum -> _ -> AggregatedAccum
aggregate acc conf =
  (bimap firstAgg secondAgg) <$> acc
  where
  firstAgg = runAggregation (conf ^._firstAggregation)
  secondAgg = runAggregation (conf ^._secondAggregation)


extractClean :: VizRec -> _ -> AggregatedAccum
extractClean r conf =
  aggregate (extractData r conf) conf

getXAxisType :: VizRec -> _ -> AxisType
getXAxisType r conf =
  case (conf ^._dims.._selection) >>= (flip lookup (r ^._all)) of
    Just (Me.TimeAxis _) -> TimeAxis
    _ -> CategoryAxis

mkSeries :: Boolean -> AxisType -> AggregatedAccum -> Tuple Axises (Array Series)
mkSeries needTwoAxis ty acc =
  Tuple xAxis series
  where
  ks :: Array Key
  ks = L.fromList $ keys acc

  series :: Array Series
  series =
    case group of
      Tuple firsts seconds ->
        L.fromList $
        (firstSerie <$> toList firsts) <>
        (if needTwoAxis
         then secondSerie <$> toList seconds
         else L.Nil)

  catVals :: Array String
  catVals = nub $ keyCategory <$> ks

  xAxis = OneAxis $ Axis
          axisDefault { "type" = Just ty
                      , "data" = Just $ CommonAxisData <$> catVals
                      }

  serie :: Number -> Tuple String (Array Number) -> Series
  serie ix (Tuple name nums) =
    LineSeries { common: if name == ""
                         then universalSeriesDefault
                         else universalSeriesDefault { "name" = Just name }
               , lineSeries: lineSeriesDefault { "data" = Just $ simpleData <$> (nums)
                                               , yAxisIndex = Just ix
                                               }
               }
  firstSerie :: Tuple String (Array Number) -> Series
  firstSerie = serie 0.0

  secondSerie :: Tuple String (Array Number) -> Series
  secondSerie = serie 1.0

  group :: Tuple (Map String (Array Number)) (Map String (Array Number))
  group =
    bimap nameMap nameMap $
    splitSeries $ L.fromList $ toList acc

  splitSeries :: Array (Tuple Key (Tuple Number Number)) ->
                 Tuple (Array (Tuple Key Number)) (Array (Tuple Key Number))
  splitSeries src  =
    foldl (\(Tuple firsts seconds) (Tuple k (Tuple f s)) ->
            Tuple ((Tuple k f):firsts) ((Tuple k s):seconds))
    (Tuple [] []) src

  nameMap :: Array (Tuple Key Number) -> Map String (Array Number)
  nameMap = named'' <<< filled <<< named'

  named :: Array (Tuple Key Number) -> String -> Map String Number
  named lst cat =
    (fromList <<< L.toList) $
    ((\x -> lmap keyName x) <$>
     (filter (\(Tuple k _) -> keyCategory k == cat) lst))

  named' :: Array (Tuple Key Number) -> Array (Map String Number)
  named' lst = named lst <$> catVals

  namedKeys :: Array (Map String Number) -> Array String
  namedKeys ms = nub $ concat (L.fromList <<< keys <$> ms)

  filled :: Array (Map String Number) -> Array (Map String Number)
  filled ms =
    let ks = namedKeys ms in
    (\m -> foldl fill m ks) <$> ms

  fill :: Map String Number -> String ->  Map String Number
  fill m key =
    alter (\k -> case k of
              Nothing -> Just 0.0
              a -> a) key m

  named'' :: Array (Map String Number) -> Map String (Array Number)
  named'' m =
    reverse <$> (foldl foldFn empty (L.fromList <<< toList <$> m))

  foldFn :: Map String (Array Number) -> Array (Tuple String Number) -> Map String (Array Number)
  foldFn m tpls =
    foldl (\m (Tuple k n) -> alter (alterNamed n) k m) m tpls

  alterNamed :: Number -> Maybe (Array Number) -> Maybe (Array Number)
  alterNamed n ns =
    Just $ (n:(fromMaybe [] ns))


needTwoAxises :: VizRec -> _ -> Boolean
needTwoAxises r conf =
  isJust ((conf ^._secondMeasures.._selection) >>= (flip lookup (r ^._all)))

mkLine :: VizRec -> _ -> Option
mkLine r conf =
  case tpls of
    Tuple xAxis series ->
      Option optionDefault { series = Just $ Just <$> series
                           , xAxis = Just xAxis
                           , yAxis = Just yAxis
                           , tooltip = Just $ Tooltip $
                                       tooltipDefault {trigger = Just TriggerItem}
                           , legend = Just $ mkLegend series
                           }
  where

  mkLegend :: (Array Series) -> Legend
  mkLegend series =
    Legend legendDefault { "data" = Just $ legendItemDefault <$> extractNames series}

  extractNames :: (Array Series) -> Array String
  extractNames ss = catMaybes (extractName <$> ss)

  extractName :: Series -> Maybe String
  extractName (LineSeries r) = r.common.name
  extractName _ = Nothing

  xAxisType :: AxisType
  xAxisType = getXAxisType r conf

  extracted :: AggregatedAccum
  extracted = extractClean r conf

  tpls :: Tuple Axises (Array Series)
  tpls = mkSeries (needTwoAxises r conf) xAxisType extracted

  yAxis' :: Axis
  yAxis' = Axis axisDefault { "type" = Just ValueAxis}

  yAxis :: Axises
  yAxis =
    if needTwoAxises r conf
    then TwoAxises yAxis' yAxis'
    else OneAxis yAxis'
