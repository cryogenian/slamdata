module Controller.Notebook.Cell.Viz where

import Prelude 
import Api.Query (count, all, sample)
import Control.Apply ((*>))
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.MonadPlus (guard)
import Control.Plus (empty)
import Data.Argonaut.Core (JArray())
import Data.Array (range, zipWith, concat, replicate, length, filter, (!!), null)
import Data.Foldable (fold, foldl)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Time (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)

import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(..))
import Model.Notebook (State(), _notebook)
import Model.Notebook.Cell (Cell(), _content, _Visualize, _cellId, CellId(), _runState, _input, _hasRun, newVisualizeContent, RunState(..))
import Model.Notebook.Cell.Viz 
import Model.Notebook.Domain
import Model.Notebook.Port (_PortResource)
import Model.Resource (Resource())
import Optic.Core 
import Optic.Fold ((^?))
import Optic.Extended (TraversalP())
import Data.Argonaut.JCursor (JCursor())
import Data.Map (Map(), keys, toList, lookup, values)
import Data.Selection
import qualified Model.Notebook.ECharts as Me
import Global (readInt, isNaN)
import qualified Data.Set as S
import Utils (s2i, s2n)
import Controller.Notebook.Common (update, I())
import Controller.Notebook.Cell.Viz.Pie (mkPie)
import Controller.Notebook.Cell.Viz.Line (mkLine)
import Controller.Notebook.Cell.Viz.Bar (mkBar)
import qualified Model.Notebook.ECharts as Me
import ECharts.Options
import Utils (s2i)
import qualified Data.List as L

selectAgg :: forall e. Cell -> LensP VizRec Aggregation -> Aggregation -> I e
selectAgg cell _agg agg =
  (update cell upd) <>
  (updateOpts (upd cell))
  where upd = _content.._Visualize.._agg .~ agg

setChartHeight :: forall e. Cell -> String -> I e
setChartHeight cell height =
  case s2i' height of
    Nothing -> empty
    Just h ->
      (update cell (_content.._Visualize.._chartHeight .~ toNumber h)) <>
      (pure $ ResizeECharts (show $ cell ^. _cellId))
      
s2i' :: String -> Maybe Int
s2i' s = if s == "" then pure 0 else s2i s


setChartWidth :: forall e. Cell -> String -> I e
setChartWidth cell width =
  case s2i' width of
    Nothing -> empty
    Just w ->
      (update cell (_content .. _Visualize .. _chartWidth .~ toNumber w)) <>
      (pure $ ResizeECharts (show $ cell ^. _cellId))


selectChartType :: forall e. Cell -> ChartType -> I e
selectChartType cell ct =
  (update cell (_content.._Visualize.._chartType .~ ct)) <>
  (updateOpts (cell # _content.._Visualize.._chartType .~ ct))

cleanChart :: forall e. Cell -> I e
cleanChart cell =
  pure $ SetEChartsOption (show $ cell ^._cellId) (Option optionDefault)

insertViz :: forall e. State -> Cell -> I e
insertViz state parent =
  case insertCell parent newVisualizeContent (state ^. _notebook) of
    Tuple note cell ->
      (pure $ WithState (_notebook .~ note)) 
      `andThen` \_ ->
      (updateInserted cell)
      `andThen` \_ ->
      (pure ForceSave)

runViz :: forall e. Cell -> I e
runViz cell =
  (updateInserted cell)
  `andThen` \_ ->
  (update cell (_runState .~ RunFinished (Milliseconds 0)))


updateViz :: forall e. Cell -> VizRec -> I e
updateViz cell r =
  (update cell (_content.._Visualize .~ configured)) <>
  (updateOpts (cell # _content.._Visualize .~ configured))
  where
  configured = configure r


updateInserted :: forall e. Cell -> I e
updateInserted cell =
  maybe errorInPort (updateData cell) $ cell ^? _input .. _PortResource   
  where
  error :: String -> I e
  error msg = update cell (_content.._Visualize.._error .~ msg)

  errorInPort :: I e
  errorInPort = error "Incorrect input port"

updateData :: forall e. Cell -> Resource -> I e
updateData cell file = do
  numItems <- liftAff $ count file
  if numItems < 1
    then errorEmptyInput
    else do
    sample <- Me.analyzeJArray <$>
              (liftAff $ sample file 0 20)

    all <- Me.analyzeJArray <$> (liftAff $ all file)

    let vizRec = fromMaybe initialVizRec $ cell ^? _content.._Visualize
        axes = keys all
    if L.null axes
      then updateOpts cell
      else 
      let vRec = configure $ (vizRec # _all .~ all
                                     # _sample .~ sample
                             )
          vRec' = vRec # _error .~ if S.isEmpty (vRec ^._availableChartTypes)
                                   then "There is no availbale chart type for this data"
                                   else ""


      in (update cell ((_content .. _Visualize .~ vRec')
                    .. (_hasRun .~ true))) <> 
         (updateOpts (cell # _content.._Visualize .~ vRec'))

  where
  errored :: String -> I e
  errored msg = update cell (_content.._Visualize.._error .~ msg) 
  
  errorEmptyInput :: I e
  errorEmptyInput = errored "Empty input"

configure :: VizRec -> VizRec
configure r =
  let tpls = L.fromList $ toList (r ^._sample)
      cats = fst <$> (filter (snd >>> Me.isCatAxis) tpls)
      vals = fst <$> (filter (snd >>> Me.isValAxis) tpls)
      times = fst <$> (filter (snd >>> Me.isTimeAxis) tpls)
      
      pie = (r ^._pieConfiguration) #
            (_cats %~ autoSelect) ..
            (_firstMeasures %~ autoSelect) ..
            (_cats.._variants .~ cats) ..
            (_firstSeries.._variants .~ ((cats <-> p.cats) `onlyIfSelected` p.cats)) ..
            (_secondSeries.._variants .~ ((cats <-> p.cats <-> p.firstSeries) `onlyIfSelected` p.firstSeries)) ..
            (_firstMeasures.._variants .~ (depends p.cats vals))

             

      bar = (r ^._barConfiguration) #
            (_cats %~ autoSelect) ..
            (_firstMeasures %~ autoSelect) ..
            (_cats.._variants .~ cats) ..
            (_firstSeries.._variants .~ ((cats <-> b.cats) `onlyIfSelected` b.cats)) ..
            (_secondSeries.._variants .~ ((cats <-> b.cats <-> b.firstSeries) `onlyIfSelected` b.firstSeries)) ..
            (_firstMeasures.._variants .~ (depends b.cats vals))


      line = (r ^._lineConfiguration) #
             (_dims %~ autoSelect) ..
             (_firstMeasures %~ autoSelect) ..
             (_dims.._variants .~ (times <> cats)) ..
             (_firstSeries.._variants .~ ((cats <-> l.dims) `onlyIfSelected` l.dims)) ..
             (_secondSeries.._variants .~ ((cats <-> l.dims <-> l.firstSeries) `onlyIfSelected` l.firstSeries)) ..
             (_firstMeasures.._variants .~ (depends l.dims vals)) ..
             (_secondMeasures.._variants .~ ((depends l.dims $(vals <-> l.firstMeasures)) `onlyIfSelected` l.firstMeasures))

      available = if null vals
                  then L.Nil
                  else if not $ null cats
                       then L.toList [Pie, Bar, Line]
                       else if null times
                            then L.Nil
                            else L.singleton Line
      error = if L.null available then "No available chart types, please, rerun cell" else ""
      
      

  in r # _pieConfiguration .~ pie
       # _barConfiguration .~ bar
       # _lineConfiguration .~ line
       # _availableChartTypes .~ S.fromList available
  where
    p = r ^._pieConfiguration
    b = r ^._barConfiguration
    l = r ^._lineConfiguration

    onlyIfSelected :: forall a. Array a -> Selection a -> Array a
    onlyIfSelected lst sel = maybe [] (const lst) (sel ^._selection)

updateOpts :: forall e. Cell -> I e
updateOpts cell =
  -- options event with true in updateOptio in echarts
  -- don't update properly :(
  (setOutput cell) `andThen` \_ ->
  (maybe empty go (cell ^? _content.._Visualize))
  where
  go r = do
    let opts = case r ^._chartType of
          Pie -> mkPie r (r ^. _pieConfiguration) 
          Line -> mkLine r (r ^. _lineConfiguration)
          Bar -> mkBar r (r ^. _barConfiguration)
    if L.null $ keys (r ^._all) 
      then empty :: I e
      else
      (update cell (_content.._Visualize.._output .~ opts)) `andThen` \_ ->
      (setOutput (cell # _content.._Visualize.._output .~ opts))


setOutput :: forall e. Cell -> I e
setOutput cell =
  (cleanChart cell) `andThen` \_ ->
  (maybe empty go (cell ^? _content.._Visualize.._output))
  where
  go opts =
    pure $ SetEChartsOption (show $ cell ^._cellId) opts
