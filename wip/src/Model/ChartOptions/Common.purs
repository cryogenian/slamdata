module Model.ChartOptions.Common where

import Prelude

import Control.Bind ((>=>), join)
import Data.Argonaut (JCursor())
import Data.Array (catMaybes, cons, (!!))
import Data.Lens (view)
import Data.List (List(..), replicate, length)
import Data.Maybe (fromMaybe, maybe, Maybe(..))
import Data.Map as M
import Model.ChartConfiguration (ChartConfiguration(..), JSelect())
import Model.ChartSemantics (Semantics(), printSemantics, semanticsToNumber)
import Model.Aggregation (Aggregation(..), runAggregation)
import Model.Select (_value)
import Model.ChartAxis as Ax
import Data.Tuple (Tuple(..), fst, snd)

type ChartAxises =
  { dimensions :: Array (List (Maybe String))
  , series :: Array (List (Maybe String))
  , measures :: Array (List (Maybe Number))
  , aggregations :: Array (Maybe Aggregation)
  }

buildChartAxises :: M.Map JCursor Ax.Axis -> ChartConfiguration -> ChartAxises
buildChartAxises axisMap (ChartConfiguration conf) =
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
