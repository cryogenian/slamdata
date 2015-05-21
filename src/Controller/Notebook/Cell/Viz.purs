module Controller.Notebook.Cell.Viz where

import Data.Maybe (Maybe(..))
import Optic.Core ((^.), (.~), (..))
import Data.Tuple
import Control.Plus (empty)
import Data.Foldable (fold)
import Data.Array (range, zipWith, concat, replicate)

import Controller.Notebook.Common (I())
import Model.Notebook (State(), _notebook)
import Model.Notebook.Domain 
import Model.Notebook.Cell (Cell(), CellContent(Visualize), _cellId, _runState, RunState(..), newVisualizeContent, _content, _Visualize)
import Model.Notebook.Cell.Viz (initialVizRec, _baseSelected, _output, baseOptsLabel, miniatureLabel, resultLabel)
import Input.Notebook (Input(..))

import ECharts.Chart
import ECharts.Options
import ECharts.Tooltip
import ECharts.Toolbox
import ECharts.Coords
import ECharts.Legend
import ECharts.Axis
import ECharts.Series
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Item
import ECharts.Grid 

import Data.Time (Milliseconds(..))


import Halogen.HTML.Events.Monad (andThen)

insertViz :: forall e. State -> Cell -> I e
insertViz state parent =
  case insertCell parent cont (state ^. _notebook) of
    Tuple note cell ->
      (pure $ WithState (_notebook .~ note)) 
      `andThen` \_ -> updateCharts (show $ cell ^. _cellId) 
  where
  baseOpts = replicate 4 opts
  bases cid = (cid <>) <$> (baseOptsLabel <>) <$> show <$> range 0 3
  baseMsgs :: String -> [I e]
  baseMsgs cid = pure <$> zipWith SetEChartsOption (bases cid) baseOpts

  miniOpts = replicate 24 opts
  minis cid = (cid <>) <$> (miniatureLabel <>) <$> show <$> range 0 23
  miniMsgs :: String -> [I e]
  miniMsgs cid = pure <$> zipWith SetEChartsOption (minis cid) miniOpts 

  resultMsg :: String -> I e
  resultMsg cid = pure $ SetEChartsOption (cid <> resultLabel) opts

  updateCharts :: String -> I e
  updateCharts cid = fold $ concat [miniMsgs cid, [resultMsg cid], baseMsgs cid] 
  
  cont = Visualize initialVizRec{ miniatures = miniOpts
                                , baseOptions = baseOpts }

runViz :: forall e. Cell -> I e
runViz cell =
  (pure $ SetEChartsOption (show $ cell ^. _cellId) opts)
  `andThen` \_ ->
  (pure $ update (_runState .~ RunFinished (Milliseconds 0)))
  where
  update :: (Cell -> Cell) -> Input 
  update = UpdateCell (cell ^. _cellId)

previewSelected :: forall e. Cell -> Option -> I e
previewSelected cell opts =
  (update $
   (_content .. _Visualize .. _baseSelected .~ true) ..
   (_content .. _Visualize .. _output .~ opts))
  `andThen` \_ ->
  (pure $ SetEChartsOption (show $ cell ^. _cellId) opts)
  where
  update = pure <<< (UpdateCell (cell ^. _cellId))



opts :: Option
opts = Option $ optionDefault { xAxis = xAxis
                              , yAxis = yAxis
                              , grid = grid 
                              , series = series
                              }
  where
  grid = Just $ Grid $
         gridDefault { x = Just $ Pixel 10
                     , x2 = Just $ Pixel 10
                     , y = Just $ Pixel 10
                     , y2 = Just $ Pixel 10
                     }
  xAxis = Just $ OneAxis $ Axis $
          axisDefault { "type" = Just CategoryAxis
                      , boundaryGap = Just $ CatBoundaryGap false
                      , "data" = Just $ CommonAxisData <$> [ "Monday"
                                                           , "Tuesday"
                                                           , "Wednesday"
                                                           , "Thursday"
                                                           , "Friday"
                                                           , "Saturday"
                                                           , "Sunday"
                                                           ]
                      , axisLabel = hideLabel
                      }
  hideLabel = Just $ AxisLabel $ axisLabelDefault {show = Just false}
  yAxis = Just $ OneAxis $ Axis $ axisDefault { "type" = Just ValueAxis
                                              , axisLabel = hideLabel}
  series = Just $ Just <$>
           [ LineSeries { common: universalSeriesDefault
                          { name = Just "email marketing"
                          }
                        , lineSeries: lineSeriesDefault
                          { stack = Just "total"
                          , "data" = Just $ Value <<< Simple <$>
                                     [120, 132, 101, 134, 90, 230, 210]
                          , smooth = Just true
                          }
                        }
             ] 
