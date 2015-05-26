module Controller.Notebook.Cell.Viz where


import Api.Query (count, all, sample)
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
import Model.Notebook.Cell (Cell(), CellContent(..), CellId(), _cellId, _runState, RunState(..), newVisualizeContent, _content, _Visualize, _input)
import Model.Notebook.Cell.Viz (VizRec(), initialVizRec, _error, _output, _xs, _all, _ys, _yCursor, _xCursor, _selectedFlag, ChartType(..), _chartType)
import Model.Notebook.Domain
import Model.Notebook.Port (_PortResource)
import Model.Resource (Resource())
import Optic.Core ((^.), (.~), (..), (%~))
import Optic.Fold ((^?))
import Optic.Extended (TraversalP())
import Data.Argonaut.JCursor (JCursor())
import Data.Map (Map(), keys, toList, lookup)
import qualified Model.Notebook.ECharts as Me
import Global (readInt, isNaN)

handleChartType :: forall e. ChartType -> Cell -> I e
handleChartType chartType cell = 
  (update (_content.._Visualize.._chartType .~ chartType)) `andThen` \_ -> 
  maybe empty (updateOpts cell <<< (_chartType .~ chartType)) (cell ^? _content.._Visualize)
  where
  cid :: CellId
  cid = cell ^._cellId
  update :: (Cell -> Cell) -> I e
  update = pure <<< (UpdateCell cid)

updateOpts :: forall e. Cell -> VizRec -> I e
updateOpts cell r =
  let opts = mkOption r
      cid = cell ^._cellId
  in 
   (pure (UpdateCell cid (_content.._Visualize.._output .~ opts))) <>
   (pure (SetEChartsOption (show cid) opts))
  
handleAxis :: forall e. TraversalP Cell [JCursor] ->
              TraversalP Cell (Maybe JCursor) ->
              String -> Cell -> I e
handleAxis _xys _cursor ix cell =
  maybe empty go (cell ^? _content.._Visualize.._selectedFlag)
  where
  update :: (Cell -> Cell) -> I e
  update = pure <<< (UpdateCell (cell ^._cellId))

  go :: Boolean -> I e
  go flag =
    (update (_content.._Visualize.._selectedFlag %~ not)) `andThen` \_ ->
    case availableAxises ix cell of
      Tuple mbCursor xys ->
        update (_cursor .~ mbCursor) `andThen` \_ ->
        if flag then empty
        else update (_xys .~ xys)
       
handleXAxisSelected :: forall e. String -> Cell -> I e
handleXAxisSelected =
  handleAxis (_content.._Visualize.._ys) (_content.._Visualize.._xCursor)

handleYAxisSelected :: forall e. String -> Cell -> I e
handleYAxisSelected =
  handleAxis (_content.._Visualize.._xs) (_content.._Visualize.._yCursor)
                     
availableAxises :: String -> Cell -> Tuple (Maybe JCursor) [JCursor]
availableAxises ix cell =
  Tuple mbCursor (maybe ks cursors $ mbCursor)
  where
  mbCursor :: Maybe JCursor 
  mbCursor = s2n ix >>= (ks !!)
  
  s2n :: String -> Maybe Number 
  s2n s =
    let n = readInt 10 s in 
    if isNaN n
    then Nothing
    else Just n
         
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
        vRec = vizRec { all = all
                      , sample = sample
                      , ys = axes
                      , xs = axes
                      }
        output = mkOption vRec
        newVizRec = vRec{output = output}
    (update $ (_content .. _Visualize .~ vRec)) <>
    (updateOpts cell vRec)
  where
  errorEmptyInput :: I e
  errorEmptyInput = update (_content.._Visualize.._error .~ "Empty input")

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
mkLinear series r =
  Option $ optionDefault { xAxis = xAxis (fst <$> preparedData)
                         , yAxis = yAxis
                         , series = series (snd <$> preparedData)
                         }
  where
  
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

