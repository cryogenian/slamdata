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

module SlamData.Notebook.Cell.Chart.ChartOptions.Common where

import Prelude

import Control.Bind ((>=>), join)

import Data.Argonaut (JCursor())
import Data.Array (catMaybes, cons, (!!))
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Lens (view)
import Data.List (List(..), replicate, length)
import Data.List as L
import Data.Map (Map())
import Data.Map as M
import Data.Maybe (fromMaybe, maybe, Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)

import ECharts

import SlamData.Form.Select (_value)
import SlamData.Notebook.Cell.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Notebook.Cell.Chart.Axis as Ax
import SlamData.Notebook.Cell.Chart.ChartConfiguration (ChartConfiguration(), JSelect())
import SlamData.Notebook.Cell.Chart.Semantics (Semantics(), printSemantics, semanticsToNumber)

type ChartAxises =
  { dimensions :: Array (List (Maybe String))
  , series :: Array (List (Maybe String))
  , measures :: Array (List (Maybe Number))
  , aggregations :: Array (Maybe Aggregation)
  }

colors :: Array String
colors =
  [ "#93A9A6"
  , "#CDA71F"
  , "#EB6F76"
  , "#66B35B"
  , "#BD97E2"
  , "#5B5925"
  , "#884A6F"
  , "#51B3F6"
  , "#CCA067"
  , "#398465"
  , "#3C607A"
  , "#81463C"
  , "#B65F33"
  , "#9AAE31"
  , "#CE8B9E"
  , "#6C6356"
  , "#95A779"
  , "#44AECB"
  , "#E987C2"
  , "#8A7EA0"
  , "#3D6C2F"
  , "#40B994"
  , "#87984A"
  , "#C1A088"
  , "#9B6E2D"
  , "#428C8D"
  , "#B8766A"
  , "#EB8666"
  , "#DF883E"
  , "#BB6273"
  , "#C994BF"
  , "#929DE0"
  , "#7BA5CB"
  , "#AE9093"
  , "#66557B"
  , "#936370"
  , "#3C6D64"
  , "#84693F"
  , "#C19744"
  , "#E77AA1"
  , "#5D555F"
  , "#4078A2"
  , "#3FAFAF"
  , "#698B99"
  , "#486A4C"
  , "#7EA48B"
  , "#B57E57"
  , "#8C72AA"
  , "#609050"
  , "#56B379"
  , "#489F8B"
  , "#714D4A"
  , "#9A8867"
  , "#93B66B"
  , "#7DA93F"
  , "#877424"
  , "#C75D56"
  , "#B774AC"
  , "#7B7A3E"
  , "#73581C"
  , "#398EA3"
  , "#964734"
  , "#DF8D89"
  , "#AF97CC"
  , "#96951F"
  , "#A37791"
  , "#7C4D2E"
  , "#78865F"
  , "#216B74"
  , "#935524"
  , "#6FAFB6"
  , "#75AB76"
  , "#A48B50"
  , "#D28DD0"
  , "#BE9AAF"
  , "#AD8D22"
  , "#D89576"
  , "#964860"
  , "#9B9A61"
  , "#4DAADB"
  , "#A9628D"
  , "#98943B"
  , "#486366"
  , "#6D7E2B"
  , "#CF9A2F"
  , "#827A8B"
  , "#876A69"
  , "#495F23"
  , "#677F45"
  , "#805845"
  , "#A2544D"
  , "#8C5157"
  , "#6B6C9E"
  , "#236443"
  , "#919B82"
  , "#CC8E55"
  , "#3E8555"
  , "#A08A7A"
  , "#767870"
  , "#6D9643"
  , "#87658F"
  , "#3BB069"
  , "#6A5D42"
  , "#586249"
  , "#1F7769"
  , "#6DAF8E"
  , "#8FA7BE"
  , "#B7A82C"
  , "#A09DA0"
  , "#7D8AA6"
  , "#78A3E0"
  , "#719186"
  , "#765771"
  , "#A37EA7"
  , "#8E8CBC"
  , "#A76840"
  , "#49934B"
  , "#A27C62"
  , "#3DA27B"
  , "#A9AC53"
  , "#6685B4"
  , "#5F728A"
  , "#CB6B4A"
  , "#9F8DD3"
  , "#B7A66E"
  , "#A998B3"
  , "#85A362"
  , "#595146"
  ]

buildChartAxises :: M.Map JCursor Ax.Axis -> ChartConfiguration -> ChartAxises
buildChartAxises axisMap conf =
  { dimensions: dimensions
  , series: series
  , measures: measures
  , aggregations: aggregations
  }
  where
  dimensions :: Array (List (Maybe String))
  dimensions = map (map (map printSemantics)) $ getAxises conf.dimensions

  series :: Array (List (Maybe String))
  series = map (map (map printSemantics)) $ getAxises conf.series

  measures :: Array (List (Maybe Number))
  measures = map (map (flip bind semanticsToNumber)) $ getAxises conf.measures

  getAxises :: Array JSelect -> Array (List (Maybe Semantics))
  getAxises sels =
    map Ax.runAxis $ catMaybes $ map (view _value >=> flip M.lookup axisMap) sels

  aggregations :: Array (Maybe Aggregation)
  aggregations = map (view _value) conf.aggregations

type Key = Tuple String SeriesKey
type SeriesKey = Maybe (Tuple String (Maybe String))

keyCategory :: Key -> String
keyCategory (Tuple cat _) = cat

keyMbSeries1 :: Key -> Maybe String
keyMbSeries1 (Tuple _ mbT) = mbT >>= (pure <<< fst)

keyMbSeries2 :: Key -> Maybe String
keyMbSeries2 (Tuple _ mbT) = mbT >>= snd

mkKey :: String -> Maybe String -> Maybe String -> Key
mkKey cat f s =
  Tuple cat (f >>= \f -> pure $ Tuple f s)

keyName :: Key -> String
keyName k =
  (fromMaybe "" (keyMbSeries1 k)) <> (maybe "" (":" <>) (keyMbSeries2 k))

type LabeledPoints = M.Map Key (Array Number)
type PieBarData = M.Map Key Number

pieBarData :: ChartAxises -> PieBarData
pieBarData axises =
  aggregate agg $ pieBarRawData categories firstSeries secondSeries values M.empty
  where
  agg :: Aggregation
  agg = fromMaybe Sum $ join (axises.aggregations !! 0)

  categories :: List (Maybe String)
  categories = fromMaybe Nil $ axises.series !! 0

  values :: List (Maybe Number)
  values = fromMaybe Nil $ axises.measures !! 0

  firstSeries :: List (Maybe String)
  firstSeries = fromMaybe nothings $ axises.series !! 1

  secondSeries :: List (Maybe String)
  secondSeries = fromMaybe nothings $ axises.series !! 2

  nothings :: forall a. List (Maybe a)
  nothings = replicate (length values) Nothing

pieBarRawData
  :: List (Maybe String) -> List (Maybe String) -> List (Maybe String)
  -> List (Maybe Number) -> LabeledPoints -> LabeledPoints
pieBarRawData Nil _ _ _ acc = acc
pieBarRawData _ Nil _ _ acc = acc
pieBarRawData _ _ Nil _ acc = acc
pieBarRawData _ _ _ Nil acc = acc
pieBarRawData (Cons Nothing _) _ _ _ acc = acc
pieBarRawData (Cons (Just category) cs) (Cons mbFirstSerie fss)
  (Cons mbSecondSerie sss) (Cons mbValue vs) acc =
  pieBarRawData cs fss sss vs $ M.alter (alterFn val) key acc
  where
  key :: Key
  key = mkKey category mbFirstSerie mbSecondSerie

  val :: Number
  val = fromMaybe zero mbValue

  alterFn :: Number -> Maybe (Array Number) -> Maybe (Array Number)
  alterFn v vals = pure $ cons v $ fromMaybe [] vals

aggregate :: Aggregation -> LabeledPoints -> PieBarData
aggregate agg acc = map (runAggregation agg) acc


-- Having array of pairs Key -> Number and array of categories (String)
-- 1. drop any pair theat has no category from second argument
-- 2. group by category
-- 3. apply first argument to groupped maps
-- 4. make final map from category to array of values
commonNameMap
  :: (Array (Map String Number) -> Array (Map String Number)) -> Array String
  -> Array (Tuple Key Number) -> Map String (Array Number)
commonNameMap fn catVals = mapByCategories <<< fn <<< groupByCategories
  where
  groupByCategories :: Array (Tuple Key Number) -> Array (Map String Number)
  groupByCategories arr = map (markAndFilterCategory arr) catVals

  markAndFilterCategory
    :: Array (Tuple Key Number) -> String -> Map String Number
  markAndFilterCategory arr cat =
      M.fromList
    $ L.toList
    $ map (lmap keyName)
    $ A.filter (\(Tuple k _) -> keyCategory k == cat)
    $ arr

  mapByCategories
    :: Array (Map String Number) -> Map String (Array Number)
  mapByCategories arr =
    map A.reverse $ foldl foldFn M.empty (L.fromList <<< M.toList <$> arr)

  foldFn
    :: Map String (Array Number)
    -> Array (Tuple String Number)
    -> Map String (Array Number)
  foldFn m tpls = foldl (\m (Tuple k n) -> M.alter (alterNamed n) k m) m tpls

  alterNamed :: Number -> Maybe (Array Number) -> Maybe (Array Number)
  alterNamed n ns = Just $ A.cons n $ fromMaybe [] ns

mixAxisLabelAngleAndFontSize :: Int -> Int -> AxisRec -> AxisRec
mixAxisLabelAngleAndFontSize angle size r =
  r { axisLabel = Just $ AxisLabel axisLabelDefault
      { rotate = Just $ toNumber angle
      , textStyle = Just $ TextStyle textStyleDefault
        { fontSize = Just $ toNumber size
        }
      }
    }
