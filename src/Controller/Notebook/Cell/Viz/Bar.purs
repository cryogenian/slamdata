module Controller.Notebook.Cell.Viz.Bar where

import Prelude
import Data.Maybe (Maybe(..))
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
  cats :: Array (Maybe String)
  cats =
    (>>= Me.catFromSemanthic) <$>
    (maybe [] Me.runAxis ((conf ^._cats.._selection) >>= (flip lookup (r ^._all))))


  vals :: Array (Maybe Number)
  vals =
    (>>= Me.valFromSemanthic) <$> 
    (maybe [ ] Me.runAxis ((conf ^._firstMeasures.._selection) >>= (flip lookup (r ^._all))))

  maxLen :: Int
  maxLen = length vals

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
                Array (Maybe Number) -> Accum -> Accum
extractData' cats ser1 ser2 vs acc = fromMaybe acc do
  c <- head cats >>= id
  cs <- tail cats
  mbs1 <- head ser1
  sers1 <- tail ser1
  mbs2 <- head ser2
  sers2 <- tail ser2
  mbv <- head vs
  vals <- tail vs
  let v = fromMaybe 0.0 mbv
      key = mkKey c mbs1 mbs2
  pure $ extractData' cs sers1 sers2 vals (alter (alter' v) key acc)


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
  xAxis = OneAxis $ Axis
          axisDefault { "type" = Just CategoryAxis
                      , "data" = Just $ CommonAxisData <$> catVals
                      }

  ks :: Array Key
  ks = L.fromList $ keys acc

  catVals :: Array String
  catVals = nub $ keyCategory <$> ks

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
  BarSeries { common: universalSeriesDefault { name = if name == ""
                                                      then Nothing
                                                      else Just name
                                             }

            , barSeries: barSeriesDefault { "data" = Just $ simpleData <$> (nums)
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
                           , tooltip = Just $ Tooltip $
                                       tooltipDefault {trigger = Just TriggerAxis}
                           , legend = Just $ mkLegend series 
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
