module Model.ChartOptions.Bar where

import Prelude

import Data.Argonaut (JCursor())
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Foldable (foldl)
import Data.List as L
import Data.Map (Map())
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe, maybe)
import Data.String (split)
import Data.Tuple (Tuple(..))
import ECharts
import Model.ChartAxis as Ax
import Model.ChartConfiguration (ChartConfiguration())
import Model.ChartOptions.Common

buildBar :: M.Map JCursor Ax.Axis -> ChartConfiguration -> Option
buildBar axises conf = case axisSeriesPair of
  Tuple xAxis series ->
    Option optionDefault { series = Just $ map Just series
                         , xAxis = Just xAxis
                         , yAxis = Just yAxis
                         , tooltip = Just tooltip
                         , legend = Just $ mkLegend series
                         }
  where
  tooltip :: Tooltip
  tooltip = Tooltip $ tooltipDefault { trigger = Just TriggerAxis }

  mkLegend :: Array Series -> Legend
  mkLegend ss =
    Legend legendDefault { "data" = Just $ map legendItemDefault $ extractNames ss }

  extractNames :: Array Series -> Array String
  extractNames ss = A.catMaybes $ map extractName ss

  extractName :: Series -> Maybe String
  extractName (BarSeries r) = r.common.name
  extractName _ = Nothing

  axisSeriesPair :: Tuple Axises (Array Series)
  axisSeriesPair = mkSeries extracted

  extracted :: PieBarData
  extracted = pieBarData $ buildChartAxises axises conf

  yAxis :: Axises
  yAxis = OneAxis $ Axis $ axisDefault { "type" = Just ValueAxis }

mkSeries :: PieBarData -> Tuple Axises (Array Series)
mkSeries pbData = Tuple xAxis series
  where
  xAxis :: Axises
  xAxis = OneAxis $ Axis
          axisDefault { "type" = Just CategoryAxis
                      , "data" = Just $ map CommonAxisData catVals
                      }

  keysArray :: Array Key
  keysArray = L.fromList $ M.keys pbData

  catVals :: Array String
  catVals = A.nub $ map keyCategory keysArray

  series :: Array Series
  series = map serie $ L.fromList $ M.toList group

  serie :: Tuple String (Array Number) -> Series
  serie (Tuple name nums) =
    BarSeries { common: universalSeriesDefault
                  { name = if name == "" then Nothing else Just name }
              , barSeries: barSeriesDefault
                  { "data" = Just $ map simpleData nums
                  , stack = Just $ "total " <> stackFromName name
                  }
              }
  stackFromName :: String -> String
  stackFromName str = case split ":" str of
    [x, _, _] -> x
    _ -> ""

  simpleData :: Number -> ItemData
  simpleData n = Value $ Simple n

  group :: Map String (Array Number)
  group = nameMap $ L.fromList $ M.toList pbData

  nameMap :: Array (Tuple Key Number) -> Map String (Array Number)
  nameMap = commonNameMap fillEmpties catVals

  arrKeys :: Array (Map String Number) -> Array String
  arrKeys ms = A.nub $ A.concat (L.fromList <<< M.keys <$> ms)

  fillEmpties :: Array (Map String Number) -> Array (Map String Number)
  fillEmpties ms =
    let ks = arrKeys ms
    in map (\m -> foldl fill m ks) ms

  fill :: Map String Number -> String -> Map String Number
  fill m key = M.alter (maybe (Just 0.0) Just) key m
