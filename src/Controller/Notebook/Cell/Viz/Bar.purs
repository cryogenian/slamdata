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

module Controller.Notebook.Cell.Viz.Bar where

import Prelude
import Data.Maybe (Maybe(..))
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

import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple (fst, snd, Tuple(..))
import Data.Array (filter, zipWith, replicate, length, (!!), reverse, nub, concat, catMaybes, head, tail, (:))
import Data.Map (lookup, Map(), alter, values, empty, keys, toList, fromList)
import qualified Model.Notebook.ECharts as Me
import Optic.Core
import Data.Foldable (foldl)
import Data.Argonaut.JCursor (JCursor())
import Controller.Notebook.Cell.Viz.Key
import Data.Bifunctor (lmap)
import Data.String (split)
import qualified Data.List as L

simpleData = Value <<< Simple

type Accum = Map Key (Array Number)
type AggregatedAccum = Map Key Number

extractData :: VizRec -> _ -> Accum
extractData r conf =
  extractData' cats sers1 sers2 vals empty
  where
  cats :: L.List (Maybe String)
  cats =
    map (flip bind Me.catFromSemantics)
    $ maybe L.Nil Me.runAxis
    $ (conf ^._cats.._selection)
    >>= (flip lookup (r ^._all))


  vals :: L.List (Maybe Number)
  vals =
    map (flip bind Me.valFromSemantics)
    $ maybe L.Nil Me.runAxis
    $ (conf ^._firstMeasures.._selection)
    >>= flip lookup (r ^._all)

  maxLen :: Int
  maxLen = L.length vals

  nothings :: forall a. L.List (Maybe a)
  nothings = L.replicate maxLen Nothing

  sers1 :: L.List (Maybe String)
  sers1 =
    map (flip bind Me.catFromSemantics)
    $ maybe nothings Me.runAxis
    $ (conf ^._firstSeries.._selection)
    >>= flip lookup (r ^. _all)


  sers2 :: L.List (Maybe String)
  sers2 =
    map (flip bind Me.catFromSemantics)
    $ maybe nothings Me.runAxis
    $ (conf ^._secondSeries.._selection)
    >>= flip lookup (r ^. _all)


extractData' :: L.List (Maybe String) -> L.List (Maybe String) ->
                L.List (Maybe String) -> L.List (Maybe Number) -> Accum -> Accum
extractData' L.Nil _ _ _ acc = acc
extractData' _ L.Nil _ _ acc = acc
extractData' _ _ L.Nil _ acc = acc
extractData' _ _ _ L.Nil acc = acc
extractData' (L.Cons Nothing _) _ _ _ acc = acc
extractData' (L.Cons (Just c) cs) (L.Cons mbs1 sers1)
  (L.Cons mbs2 sers2) (L.Cons mbv vals) acc =
  let key = mkKey c mbs1 mbs2
      v = fromMaybe 0.0 mbv
  in extractData' cs sers1 sers2 vals (alter (alter' v) key acc)

alter' :: Number -> Maybe (Array Number) -> Maybe (Array Number)
alter' v vals =
  Just (v:(fromMaybe [] vals ))

aggregate :: Accum -> _ -> AggregatedAccum
aggregate acc conf =
  agg <$> acc
  where agg = runAggregation (conf ^._firstAggregation)

extractClean :: VizRec -> _ -> AggregatedAccum
extractClean r conf =
  aggregate (extractData r conf) conf


mkSeries :: AggregatedAccum -> Tuple Axises (Array Series)
mkSeries acc =
  Tuple xAxis series
  where
  xAxis :: Axises
  xAxis = OneAxis $ Axis axisDefault
          { "type" = Just CategoryAxis
          , "data" = Just $ CommonAxisData <$> catVals
          , axisLabel = Just $ AxisLabel axisLabelDefault
            { rotate = Just 30.0
            }
          , axisTick = Just $ AxisTick axisTickDefault
            { interval = Just $ Custom zero
            }
          }

  keysArray :: Array Key
  keysArray = L.fromList $ keys acc

  catVals :: Array String
  catVals = nub $ keyCategory <$> keysArray

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

  foldFn :: Map String (Array Number) -> Array (Tuple String Number) ->
            Map String (Array Number)
  foldFn m tpls =
    foldl (\m (Tuple k n) -> alter (alterNamed n) k m) m tpls

  alterNamed :: Number -> Maybe (Array Number) -> Maybe (Array Number)
  alterNamed n ns =
    Just $ (n:(fromMaybe [] ns))

  group :: Map String (Array Number)
  group = nameMap $ L.fromList $ toList acc

  serie :: Tuple String (Array Number) -> Series
  serie (Tuple name nums) =
  BarSeries { common: universalSeriesDefault
                      { name = if name == ""
                               then Nothing
                               else Just name
                      }
            , barSeries: barSeriesDefault
                         { "data" = Just $ simpleData <$> (nums)
                         , stack = Just $ "total" <> stackFromName name
                         }
            }
  stackFromName :: String -> String
  stackFromName str =
    case split ":" str of
      [x, _, _] -> x
      _ -> ""


  series :: Array Series
  series =
    serie <$> (L.fromList $ toList group)

mkBar :: forall e. VizRec -> _ -> Option
mkBar r conf =
  case tpls of
    Tuple xAxis series ->
      Option optionDefault { series = Just $ Just <$> series
                           , xAxis = Just xAxis
                           , yAxis = Just yAxis
                           , tooltip = Just $ Tooltip $ tooltipDefault
                             { trigger = Just TriggerAxis
                             }
                           , legend = Just $ mkLegend series
                           , grid = Just $ Grid gridDefault
                             { y2 = Just $ Percent 15.0
                             }
                           }

  where
  mkLegend :: Array Series -> Legend
  mkLegend series =
    Legend legendDefault { "data" = Just $ legendItemDefault <$> extractNames series}

  extractNames :: Array Series -> Array String
  extractNames ss = catMaybes (extractName <$> ss)

  extractName :: Series -> Maybe String
  extractName (BarSeries r) = r.common.name
  extractName _ = Nothing

  tpls :: Tuple Axises (Array Series)
  tpls = mkSeries extracted

  extracted :: AggregatedAccum
  extracted = extractClean r conf

  yAxis :: Axises
  yAxis = OneAxis $ Axis axisDefault { "type" = Just ValueAxis }
