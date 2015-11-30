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

module Controller.Notebook.Cell.Viz where

import Prelude
import qualified Api.Query as Quasar
import Control.Apply ((*>))
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.MonadPlus (guard)
import Control.Plus (empty)
import Data.Argonaut.Core (JArray())
import Data.Array (range, zipWith, concat, replicate, filter, (!!), null, length)
import Data.Foldable (fold, foldl)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust, isNothing)
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid.Disj (runDisj, Disj(..))
import Data.Time (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)

import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(..))
import Model.Notebook (State(), _notebook)
import Model.Notebook.Cell (Cell(), _content, _Visualize, _cellId, CellId(), _runState, _input, _hasRun, newVisualizeContent, RunState(..))
import Model.Notebook.Cell.Viz
import Model.Notebook.Domain hiding (_path, _name)
import Model.Notebook.Port (_PortResource)
import Model.Resource
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
      (update cell (_content.._Visualize.._chartHeight .~ h)) <>
      (pure $ ResizeECharts (show $ cell ^. _cellId))

s2i' :: String -> Maybe Int
s2i' s = if s == "" then pure 0 else s2i s


setChartWidth :: forall e. Cell -> String -> I e
setChartWidth cell width =
  case s2i' width of
    Nothing -> empty
    Just w ->
      (update cell (_content .. _Visualize .. _chartWidth .~ w)) <>
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
  (update cell (_runState .~ RunFinished zero))


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
  jarr <- liftAff $ Quasar.sample file (Just 0) (Just 20)
  if null jarr
    then errorEmptyInput
    else do
    let sample = Me.analyzeJArray jarr
    records <- liftAff $ Quasar.sample file Nothing Nothing
    if length records > 10000
      then errorTooLarge
      else do
      let all = Me.analyzeJArray records
          vizRec = fromMaybe initialVizRec $ cell ^? _content .. _Visualize
          axes = keys all
      if L.null axes
        then updateOpts cell
        else
        let vRec = configure $ (vizRec # _all .~ all
                                # _sample .~ sample
                               )
            vRec' = vRec # _error .~ if S.isEmpty (vRec ^. _availableChartTypes)
                                     then "There is no available chart type for this data"
                                     else ""
        in (update cell ((_content .. _Visualize .~ vRec')
                         .. (_hasRun .~ true))) <>
           (updateOpts (cell # _content .. _Visualize .~ vRec'))
  where
  errored :: String -> I e
  errored msg = update cell (_content.. _Visualize .. _error .~ msg)

  errorEmptyInput :: I e
  errorEmptyInput = errored "Empty input"

  errorTooLarge :: I e
  errorTooLarge = errored "Maximum record count available for visualization -- 10000, please consider to use 'limit' or 'group by' in your request"

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
            (_firstSeries.._variants .~ (ifSelected [p.cats] (cats <-> p.cats))) ..
            (_secondSeries.._variants .~ (ifSelected [p.cats, p.firstSeries] (cats <-> p.cats <-> p.firstSeries))) ..
            (_firstMeasures.._variants .~ (depends p.cats vals))



      bar = (r ^._barConfiguration) #
            (_cats %~ autoSelect) ..
            (_firstMeasures %~ autoSelect) ..
            (_cats.._variants .~ cats) ..
            (_firstSeries.._variants .~ (ifSelected [b.cats] (cats <-> b.cats))) ..
            (_secondSeries.._variants .~ (ifSelected [b.cats, b.firstSeries] (cats <-> b.cats <-> b.firstSeries))) ..
            (_firstMeasures.._variants .~ (depends b.cats vals))


      line = (r ^._lineConfiguration) #
             (_dims %~ autoSelect) ..
             (_firstMeasures %~ autoSelect) ..
             (_dims.._variants .~ (times <> cats)) ..
             (_firstSeries.._variants .~ (ifSelected [l.dims] (cats <-> l.dims))) ..
             (_secondSeries.._variants .~ (ifSelected [l.dims, l.firstSeries] (cats <-> l.dims <-> l.firstSeries))) ..
             (_firstMeasures.._variants .~ (depends l.dims vals)) ..
             (_secondMeasures.._variants .~ (ifSelected [l.firstMeasures] (depends l.dims (vals <-> l.firstMeasures))))

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

    ifSelected :: forall a. Array (Selection a) -> Array a -> Array a
    ifSelected sels lst = if runDisj $ fold $ map (Disj <<< isNothing <<< (^._selection)) sels
                          then [ ]
                          else lst

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
