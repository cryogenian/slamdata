module Controller.Notebook.Cell.Viz where

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
import Data.Int (Int(), fromNumber, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Time (Milliseconds(..))
import Data.Tuple

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
import Controller.Notebook.Common (update, I())
import Controller.Notebook.Cell.Viz.Pie (mkPie)
import Controller.Notebook.Cell.Viz.Line (mkLine)
import Controller.Notebook.Cell.Viz.Bar (mkBar)
import qualified Model.Notebook.ECharts as Me

import ECharts.Options

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
      updateInserted cell 

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
              (liftAff $ sample file (fromNumber 0) (fromNumber 20))
    all <- Me.analyzeJArray <$> (liftAff $ all file)
    let vizRec = fromMaybe initialVizRec $ cell ^? _content.._Visualize
        axes = keys all
        vRec = configure $ vizRec { all = all
                                  , sample = sample
                                  }
    (update cell (_content .. _Visualize .~ vRec)) <>
    (updateOpts (cell # _content.._Visualize .~ vRec))

  where
  errored :: String -> I e
  errored msg = update cell (_content.._Visualize.._error .~ msg) 
  
  errorEmptyInput :: I e
  errorEmptyInput = errored "Empty input"

  errorNoAvailableCharts :: I e
  errorNoAvailableCharts = errored "There is no availbale chart type for this data"

infix 9 <->
(<->) = except'



configure :: VizRec -> VizRec
configure r =
  let tpls = toList (r ^._sample)
      cats = fst <$> (filter (snd >>> Me.isCatAxis) tpls)
      vals = fst <$> (filter (snd >>> Me.isValAxis) tpls)
      times = fst <$> (filter (snd >>> Me.isTimeAxis) tpls)
      
      pie = (r ^._pieConfiguration) #
            (_cats.._variants .~ cats) ..
            (_firstSeries.._variants .~ ((cats <-> p.cats) `onlyIfSelected` p.cats)) ..
            (_secondSeries.._variants .~ ((cats <-> p.cats <-> p.firstSeries) `onlyIfSelected` p.firstSeries)) ..
            (_firstMeasures.._variants .~ (depends p.cats vals))

      bar = (r ^._barConfiguration) #
            (_cats.._variants .~ cats) ..
            (_firstSeries.._variants .~ ((cats <-> b.cats) `onlyIfSelected` b.cats)) ..
            (_secondSeries.._variants .~ ((cats <-> b.cats <-> b.firstSeries) `onlyIfSelected` b.firstSeries)) ..
            (_firstMeasures.._variants .~ (depends b.cats vals))

      line = (r ^._lineConfiguration) #
             (_dims.._variants .~ (times <> cats)) ..
             (_firstSeries.._variants .~ ((cats <-> l.dims) `onlyIfSelected` l.dims)) ..
             (_secondSeries.._variants .~ ((cats <-> l.dims <-> l.firstSeries) `onlyIfSelected` l.firstSeries)) ..
             (_firstMeasures.._variants .~ (depends l.dims vals)) ..
             (_secondMeasures.._variants .~ ((depends l.dims $(vals <-> l.firstMeasures)) `onlyIfSelected` l.firstMeasures))
      available = if null vals
                  then []
                  else if not $ null cats
                       then [Pie, Bar, Line]
                       else if null times
                            then []
                            else [Line]
      error = if null available then "No available chart types" else ""
      
      

  in r # _pieConfiguration .~ pie
       # _barConfiguration .~ bar
       # _lineConfiguration .~ line
       # _availableChartTypes .~ S.fromList available
       # _error .~ error
  where
    p = r.pieConfiguration
    b = r.barConfiguration
    l = r.lineConfiguration

    onlyIfSelected :: forall a. [a] -> Selection a -> [a]
    onlyIfSelected lst sel = maybe [] (const lst) sel.selection
import Debug.Foreign

updateOpts :: forall e. Cell -> I e
updateOpts cell =
  -- options event with true in updateOptio in echarts
  -- don't update properly :(
  (cleanChart cell) `andThen` \_ -> 
  (maybe empty go (cell ^? _content.._Visualize))
  where
  go r = do
    let opts = case r ^._chartType of
          Pie -> mkPie r (r ^. _pieConfiguration) 
          Line -> mkLine r (r ^. _lineConfiguration)
          Bar -> mkBar r (r ^. _barConfiguration)
    pure $ SetEChartsOption (show $ cell ^._cellId) opts

