module Controller.Notebook.Cell.Viz.Bar where

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
import Data.Tuple
import Data.Array (filter, zipWith, replicate, length, (!!), reverse, nub, concat, catMaybes)
import Data.Map (lookup, Map(), alter, values, empty, keys, toList, fromList)
import qualified Model.Notebook.ECharts as Me
import Optic.Core
import Data.Foldable (foldl)
import Data.Argonaut.JCursor (JCursor())
import Controller.Notebook.Cell.Viz.Key
import Data.Bifunctor (lmap)
import Data.String (split)

simpleData = Value <<< Simple

type Accum = Map Key [Number]
type AggregatedAccum = Map Key Number

extractData :: VizRec -> _ -> Accum
extractData r conf =
  extractData' cats sers1 sers2 vals empty
  where
  cats :: [Maybe String]
  cats =
    (>>= Me.catFromSemanthic) <$>
    (maybe [] Me.runAxis ((conf ^._cats.._selection) >>= (flip lookup (r ^._all))))


  vals :: [Maybe Number]
  vals =
    (>>= Me.valFromSemanthic) <$> 
    (maybe [ ] Me.runAxis ((conf ^._firstMeasures.._selection) >>= (flip lookup (r ^._all))))

  maxLen :: Number
  maxLen = length vals

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
                [Maybe Number] -> Accum -> Accum
extractData' [] _ _ _ acc = acc
extractData' _ [] _ _ acc = acc
extractData' _ _ [] _ acc = acc
extractData' _ _ _ [] acc = acc
extractData' (mbc:cs) (mbs1:sers1) (mbs2:sers2) (mbv:vals) acc =
  extractData' cs sers1 sers2 vals $ fromMaybe acc do
    c <- mbc
    let v = fromMaybe 0 mbv
        key = mkKey c mbs1 mbs2 
    pure (alter (alter' v) key acc)

alter' :: Number -> Maybe [Number] -> Maybe [Number]
alter' v vals =
  Just (v:(fromMaybe [] vals ))

aggregate :: Accum -> _ -> AggregatedAccum 
aggregate acc conf =
  agg <$> acc
  where agg = runAggregation (conf ^._firstAggregation)

extractClean :: VizRec -> _ -> AggregatedAccum
extractClean r conf =
  aggregate (extractData r conf) conf


mkSeries :: AggregatedAccum -> Tuple Axises [Series]
mkSeries acc =
  Tuple xAxis series
  where
  xAxis :: Axises
  xAxis = OneAxis $ Axis
          axisDefault { "type" = Just CategoryAxis
                      , "data" = Just $ CommonAxisData <$> catVals
                      }

  ks :: [Key]
  ks = keys acc

  catVals :: [String]
  catVals = nub $ keyCategory <$> ks

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

  group :: Map String [Number]
  group = nameMap $ toList acc 

  serie :: Tuple String [Number] -> Series
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
      x:_:_ -> x
      _ -> ""

    
  series :: [Series]
  series =
    serie <$> (toList group)

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
  mkLegend :: [Series] -> Legend
  mkLegend series =
    Legend legendDefault { "data" = Just $ legendItemDefault <$> extractNames series}

  extractNames :: [Series] -> [String]
  extractNames ss = catMaybes (extractName <$> ss)

  extractName :: Series -> Maybe String
  extractName (BarSeries r) = r.common.name
  extractName _ = Nothing
    
  tpls :: Tuple Axises [Series]
  tpls = mkSeries extracted

  extracted :: AggregatedAccum
  extracted = extractClean r conf 
    
  yAxis :: Axises
  yAxis = OneAxis $ Axis axisDefault { "type" = Just ValueAxis }
