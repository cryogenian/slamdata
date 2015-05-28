module Controller.Notebook.Cell.Viz where

import Api.Query (count, all, sample)
import Control.Apply ((*>))
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.MonadPlus (guard)
import Control.Plus (empty)
import Controller.Notebook.Common (I())
import Data.Argonaut.Core (JArray())
import Data.Array (range, zipWith, concat, replicate, length, filter, (!!))
import Data.Foldable (fold, foldl)
import Data.Function (on)
import Data.Int (Int(), fromNumber, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Time (Milliseconds(..))
import Data.Tuple
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
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(..))
import Model.Notebook (State(), _notebook)
import Model.Notebook.Cell (Cell(), _content, _Visualize, _cellId, CellId(), _runState, _input, newVisualizeContent, RunState(..))
import Model.Notebook.Cell.Viz 
import Model.Notebook.Domain
import Model.Notebook.Port (_PortResource)
import Model.Resource (Resource())
import Optic.Core ((^.), (.~), (..), (%~))
import Optic.Fold ((^?))
import Optic.Extended (TraversalP())
import Data.Argonaut.JCursor (JCursor())
import Data.Map (Map(), keys, toList, lookup, values)
import qualified Model.Notebook.ECharts as Me
import Global (readInt, isNaN)
import qualified Data.Set as S
import Utils (s2i, s2n)

_chartOpts :: TraversalP Cell ChartOptions
_chartOpts = _content.._Visualize.._chartOptions

setBarGap :: forall e. Cell -> String -> I e
setBarGap cell gapStr =
  go $ fromMaybe 30 $ s2i gapStr
  where
  go gap =
    pure (UpdateCell (cell ^._cellId) (_chartOpts.._barGap .~ gap))

toggleSmooth :: forall e. Cell -> Boolean -> I e
toggleSmooth cell smooth =
  pure (UpdateCell (cell ^._cellId) (_chartOpts.._smooth .~ smooth))

setSymbolSize :: forall e. Cell -> String -> I e
setSymbolSize cell sizeStr =
  go $ fromMaybe 0 $ s2i sizeStr
  where
  go size =
    pure (UpdateCell (cell ^._cellId) (_chartOpts.._symbolSize .~ size))

setPieRose :: forall e. Cell -> String -> I e
setPieRose cell name =
  pure (UpdateCell (cell ^. _cellId) (_chartOpts.._roseType .~ name))

setDonutRatio :: forall e. Cell -> String -> I e
setDonutRatio cell ratioStr =
  go $ fromMaybe 0 $ s2n ratioStr
  where
  go ratio = 
    pure (UpdateCell (cell ^. _cellId) (_chartOpts.._donutRatio .~ ratio))

setMinimalAngle :: forall e. Cell -> String -> I e
setMinimalAngle cell angleStr =
  go $ fromMaybe 0 $ s2n angleStr
  where
  go angle = 
    pure (UpdateCell (cell ^. _cellId) (_chartOpts.._minimalAngle .~ angle))

setXAxisPosition :: forall e. Cell -> String -> I e
setXAxisPosition cell pos =
  pure (UpdateCell (cell ^._cellId) (_chartOpts.._xAxisPosition .~ pos))

setYAxisPosition :: forall e. Cell -> String -> I e
setYAxisPosition cell pos =
  pure (UpdateCell (cell ^._cellId) (_chartOpts.._yAxisPosition .~ pos))

handleChartType :: forall e. ChartType -> Cell -> I e
handleChartType chartType cell =
  maybe empty go $ cell ^? _content.._Visualize 
  where
  go :: VizRec -> I e
  go r =
    let newR = updateR chartType r in
    (update (_content.._Visualize .~ newR)) <>
    (updateOpts cell newR)
    
  cid :: CellId
  cid = cell ^._cellId
  update :: (Cell -> Cell) -> I e
  update = pure <<< (UpdateCell cid)

  updateR :: ChartType -> VizRec -> VizRec
  updateR chartType r =
    r #
    (_chartType .~ chartType) ..
    (_xs .~ xs chartType r) ..
    (_ys .~ ys chartType r)

  xs :: ChartType -> VizRec -> [JCursor]
  xs chartType r =
    let fn = case chartType of
          Line -> const true
          _ -> not <<< Me.isValAxis
    in fst <$> (filter (fn <<< snd) $ toList (r ^._all))

  ys :: ChartType -> VizRec -> [JCursor]
  ys chartType r =
    let fn = Me.isValAxis in
    fst <$> (filter (fn <<< snd) $ toList (r ^._all))



updateOpts :: forall e. Cell -> VizRec -> I e
updateOpts cell r =
  (pure (UpdateCell cid (_content.._Visualize.._output .~ opts))) `andThen` \_ ->
  maybe empty go $ ((r ^._xCursor) *> (r^._yCursor))

  where
  cid = cell ^._cellId
  opts = mkOption r 
  go = const $ pure (SetEChartsOption (show cid) opts)
  
handleAxis :: forall e.
              TraversalP Cell [JCursor] ->
              TraversalP Cell [JCursor] ->
              TraversalP VizRec (Maybe JCursor) ->
              String -> Cell -> I e
handleAxis _xys _yxs _cursor ix cell =
  maybe empty go $ cell ^? _content.._Visualize 
  where
  update :: (Cell -> Cell) -> I e
  update = pure <<< (UpdateCell (cell ^._cellId))

  go :: VizRec -> I e
  go r =
    case availableAxises ix cell _yxs of
      Tuple mbCursor xys ->
        let newR = r # _selectedFlag %~ not 
                     # _cursor .~ mbCursor
            cellR = cell # _content.._Visualize .~ newR 
            newCell = cellR # _xys .~ xys
        in update (const cellR) `andThen` \_ ->
        (updateOpts cell newR) `andThen` \_ -> 
        if r ^. _selectedFlag then empty
        else update (const newCell)
  
handleXAxisSelected :: forall e. String -> Cell -> I e
handleXAxisSelected =
  handleAxis (_content.._Visualize.._ys) (_content.._Visualize.._xs) _xCursor

handleYAxisSelected :: forall e. String -> Cell -> I e
handleYAxisSelected =
  handleAxis (_content.._Visualize.._xs) (_content.._Visualize.._ys) _yCursor
                     
availableAxises :: String -> Cell -> TraversalP Cell [JCursor] ->
                   Tuple (Maybe JCursor) [JCursor]
availableAxises ix cell _xys =
  Tuple mbCursor (maybe ks cursors $ mbCursor)
  where
  mbCursor :: Maybe JCursor 
  mbCursor = s2i ix >>= ((cell ^._xys) !!)
  
  m :: Map JCursor Me.Axis 
  m = cell ^. _content .. _Visualize .. _all

  ks :: [JCursor]
  ks = keys m

  cursors :: JCursor -> [JCursor]
  cursors cursor =
    maybe ks (go m cursor) $ lookup cursor m
    
  go :: Map JCursor Me.Axis -> JCursor -> Me.Axis -> [JCursor]
  go m cursor axis =
    fst <$> 
    (filter (snd >>> Me.isComplement axis) $
     filter (fst >>> Me.dependsOn cursor) $
     toList m)


insertViz :: forall e. State -> Cell -> I e
insertViz state parent =
  case insertCell parent newVisualizeContent (state ^. _notebook) of
    Tuple note cell ->
      (pure $ WithState (_notebook .~ note)) 
      `andThen` \_ ->
      updateInserted cell 

runViz :: forall e. Cell -> I e
runViz cell =
  (updateInserted cell)
  `andThen` \_ ->
  (pure $ update (_runState .~ RunFinished (Milliseconds 0)))
  where
  update :: (Cell -> Cell) -> Input 
  update = UpdateCell (cell ^. _cellId)



updateInserted :: forall e. Cell -> I e
updateInserted cell =
  maybe errorInPort (updateData cell update) $ cell ^? _input .. _PortResource   
  where
  update :: (Cell -> Cell) -> I e
  update = pure <<< (UpdateCell (cell ^._cellId))

  error :: String -> I e
  error msg = update (_content.._Visualize.._error .~ msg)

  errorInPort :: I e
  errorInPort = error "Incorrect input port"

updateData :: forall e. Cell -> ((Cell -> Cell) -> I e) -> Resource -> I e
updateData cell update file = do
  numItems <- liftAff $ count file
  if numItems < 1
    then errorEmptyInput
    else do
    sample <- Me.analyzeJArray <$>
              (liftAff $ sample file (fromNumber 0) (fromNumber 20))
    all <- Me.analyzeJArray <$> (liftAff $ all file)
    let vizRec = fromMaybe initialVizRec $ cell ^? _content.._Visualize
        axes = keys all
        vRec = mkAvailableChartTypes $ mkOutput $ vizRec { all = all
                                                         , sample = sample
                                                         , ys = axes
                                                         , xs = axes
                                                         }
    if length (S.toList vRec.availableChartTypes) == 0
      then errorNoAvailableCharts 
      else
      (update $ (_content .. _Visualize .~ vRec)) <>
      (updateOpts cell vRec)
  where
  errored :: String -> I e
  errored msg = update $ (_content.._Visualize.._error .~ msg) ..
                (_content.._Visualize.._xCursor .~ Nothing) ..
                (_content.._Visualize.._yCursor .~ Nothing)
  
  errorEmptyInput :: I e
  errorEmptyInput = errored "Empty input"

  errorNoAvailableCharts :: I e
  errorNoAvailableCharts = errored "There is no availbale chart type for this data"

mkAvailableChartTypes :: VizRec -> VizRec
mkAvailableChartTypes r = r { availableChartTypes = selectAvailableChartTypes r}

selectAvailableChartTypes :: VizRec -> S.Set ChartType
selectAvailableChartTypes r =
  S.fromList 
  (   (if pieAvailable then [Pie] else []) 
   <> (if lineAvailable then [Line] else []) 
   <> (if barAvailable then [Bar] else [])
  ) 
  where
  axises = values (r ^. _all)
  valCount = length $ filter Me.isValAxis axises
  catCount = length $ filter (\x -> Me.isTimeAxis x || Me.isCatAxis x) axises
  
  pieAvailable :: Boolean
  pieAvailable = valCount > 0 && catCount > 0

  lineAvailable :: Boolean
  lineAvailable = pieAvailable || (valCount > 1)

  barAvailable :: Boolean
  barAvailable = pieAvailable


mkOutput :: VizRec -> VizRec
mkOutput r = r { output = mkOption r}


mkOption :: VizRec -> Option
mkOption r =
  case r ^._chartType of
    Pie ->  mkPie r
    Line -> mkLine r
    Bar -> mkBar r

mkPie :: VizRec -> Option
mkPie r =
  Option $ optionDefault { series = Just [ Just series ]
                         }
  where
  series :: Series
  series =
    PieSeries { common: universalSeriesDefault
              , pieSeries: pieSeries
              }
  pieSeries :: PieSeriesRec
  pieSeries =
    pieSeriesDefault { radius = Just $ R (Percent 60)
                     , "data" = Just pieData 
                     }

  pieData :: [ItemData]
  pieData =
    fromJust <$> (filter isJust (zipWith mkPieDatum catAxis valAxis))

  catAxis :: [Maybe Me.Semanthic]
  catAxis =
    maybe [ ] Me.runAxis $ ((r ^._xCursor) >>= (\x -> lookup x (r^._all)))
  
  valAxis :: [Maybe Me.Semanthic]
  valAxis = 
    maybe [ ] Me.runAxis  $ ((r^._yCursor) >>= (\y -> lookup y (r^._all)))

  mkPieDatum :: Maybe Me.Semanthic -> Maybe Me.Semanthic -> Maybe ItemData 
  mkPieDatum cat val = do
    c <- cat
    v <- val
    n <- Me.valFromSemanthic v
    str <- Me.catFromSemanthic c
    pure $ mkPieDatum' str n

  mkPieDatum' :: String -> Number -> ItemData
  mkPieDatum' cat val = Dat $ (dataDefault $ Simple val) {name = Just cat}


mkLine :: VizRec -> Option
mkLine = mkLinear lineSeries

mkBar :: VizRec -> Option
mkBar = mkLinear barSeries 




mkLinear :: ([Number] -> Maybe [Maybe Series]) -> VizRec -> Option
mkLinear series r = Option option
  where
  option = optionDefault { series = series (snd <$> preparedData)
                                   , xAxis = xAxis (fst <$> preparedData)
                                   , yAxis = yAxis}
  yAxis :: Maybe Axises 
  yAxis = Just $ OneAxis $ Axis $ axisDefault { "type" = Just ValueAxis }
                                                         
  xAxis :: [String] ->  Maybe Axises 
  xAxis = Just <<< OneAxis <<< Axis <<< xAxisRec 

  catAxis :: [Maybe Me.Semanthic]
  catAxis =
    maybe [ ] Me.runAxis $ ((r ^._xCursor) >>= (\x -> lookup x (r^._all)))
  
  valAxis :: [Maybe Me.Semanthic]
  valAxis =
    maybe [ ] Me.runAxis  $ ((r^._yCursor) >>= (\y -> lookup y (r^._all)))

  xAxisRec :: [String] -> AxisRec
  xAxisRec strs =
    axisDefault { "type" = Just CategoryAxis
                , "data" = Just $ CommonAxisData <$> strs
                }


    

  preparedData :: [Tuple String Number]
  preparedData =
    foldl addIfCorrect [] $ (zipWith Tuple catAxis valAxis)

  addIfCorrect :: [Tuple String Number] ->
                  Tuple (Maybe Me.Semanthic) (Maybe Me.Semanthic) ->
                  [Tuple String Number]
  addIfCorrect acc (Tuple mbCat mbVal) =
    maybe acc (add mbVal) (mbCat >>= Me.catFromSemanthic)

    where add :: Maybe Me.Semanthic -> String -> [Tuple String Number]
          add mbVal str =
            (Tuple str (fromMaybe 0 $ mbVal >>= Me.valFromSemanthic)):acc


lineSeries :: [Number] -> Maybe [Maybe Series]
lineSeries nums = 
  Just [ Just $ LineSeries { common: universalSeriesDefault
                           , lineSeries: lineRec nums } ]
  where
  lineRec :: [Number] -> LineSeriesRec
  lineRec nums = 
    lineSeriesDefault { smooth = Just true
                      , "data" = Just $ Value <<< Simple <$> nums
                      }


barSeries :: [Number] -> Maybe [Maybe Series]
barSeries nums =
  Just [Just $ BarSeries { common: universalSeriesDefault
                         , barSeries: barRec nums
                         }
        ]
  where
  barRec :: [Number] -> BarSeriesRec
  barRec nums =
    barSeriesDefault {"data" = Just $ Value <<< Simple <$> nums }

