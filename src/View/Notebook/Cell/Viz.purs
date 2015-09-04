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

module View.Notebook.Cell.Viz (vizChoices, vizOutput) where

import Prelude
import Control.Plus (empty)
import Data.Maybe (Maybe(..), maybe, fromMaybe)

import qualified Data.StrMap as M
import Optic.Core
import Optic.Fold ((^?))
import Data.Selection
import Optic.Extended (TraversalP())
import Data.Array (range, zipWith, length, drop, take, (!!), null, (:))
import Data.Argonaut.JCursor (JCursor())
import Data.Int (toNumber)
import qualified Data.Set as S
import qualified Data.List as L

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.CSS as CSS

import Css.Size
import Css.Geometry (marginBottom, height, width, left, marginLeft)
import Css.String
import Css.Display

import ECharts.Options

import Model.Notebook.ECharts (Axis(..))
import Model.Notebook.Cell (Cell(), _cellId, _content, _Visualize, _runState, RunState(..))

import Controller.Notebook.Common (I())
import View.Common (row, row')
import qualified View.Css as VC
import View.Notebook.Common (HTML(), dataCellId, dataCellType, dataEChartsId)
import Data.Foreign.Class ()
import Utils.Halide (max, min, step)

-- to many imports from thiese modules to make them explicit
import Model.Notebook.Cell.Viz
import Controller.Notebook.Cell.Viz
import Utils (s2i)


vizChoices :: forall e. Cell -> HTML e
vizChoices cell =
  H.div_
  case cell ^. _runState of
    RunningSince _ -> loading
    _ -> case cell ^. _err of
      "" -> correct cell
      str -> errored str

correct :: forall e. Cell -> Array (HTML e)
correct cell =
  maybe [ ] go $ (cell ^? _content.._Visualize)
  where
  go :: VizRec -> Array (HTML e)
  go r =
    [H.div [ A.classes [ VC.vizCellEditor ] ] $
     (chartTypeSelector cell r) <>
     (chartConfiguration cell r)
    ]

chartTypeSelector :: forall e. Cell -> VizRec -> Array (HTML e)
chartTypeSelector cell r =
  [H.div [ A.classes [ VC.vizChartTypeSelector ] ]
   (img <$> (L.fromList $ S.toList  (r ^._availableChartTypes)))
  ]
  where
  img :: ChartType -> HTML e
  img ct =
    H.img [ A.src (src ct)
          , A.classes ( [ cls ct ]
                        <> (if (r ^._chartType) == ct
                            then [B.active]
                            else [])
                      )
          , E.onClick (\_ -> pure $ selectChartType cell ct)
          ]
    [ ]
  src :: ChartType -> String
  src Pie = "img/pie.svg"
  src Line = "img/line.svg"
  src Bar = "img/bar.svg"

  cls :: ChartType -> A.ClassName
  cls Pie = VC.pieChartIcon
  cls Line = VC.lineChartIcon
  cls Bar = VC.barChartIcon

chartConfiguration :: forall e. Cell -> VizRec -> Array (HTML e)
chartConfiguration cell r =
  [H.div [ A.classes [ VC.vizChartConfiguration ] ]
   [ pieConfiguration cell r (ct == Pie)
   , lineConfiguration cell r (ct == Line)
   , barConfiguration cell r (ct == Bar)
   , row
     [ H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
       [ label "Height"
       , H.input [ A.value (if zero == (r ^._chartHeight)
                            then ""
                            else show (r ^._chartHeight))
                 , A.classes [ B.formControl, VC.chartConfigureHeight ]
                 , E.onInput (\v -> pure $ setChartHeight cell v)
                 ] [ ]
       ]
     , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
       [ label "Width"
       , H.input [ A.value (if zero == (r ^._chartWidth)
                            then ""
                            else show (r ^._chartWidth))
                 , A.classes [ B.formControl, VC.chartConfigureWidth ]
                 , E.onInput (\v -> pure $ setChartWidth cell v)
                 ] [ ]
       ]
     ]
   ]
  ]
  where ct = r ^._chartType

pieConfiguration :: forall e. Cell -> VizRec -> Boolean ->  HTML e
pieConfiguration cell r visible =
  H.div [ A.classes $ [VC.pieEditor] <> (if visible then [] else [B.hide]) ]
  [ row
    [ H.form [ A.classes [ B.colXs4
                         , VC.chartConfigureForm
                         , VC.chartCategory] ]
      [ categoryLabel
      , primaryOptions cell r (_pieConfiguration .. _cats)
      ]
    , H.form [ A.classes [ B.colXs4
                         , VC.chartConfigureForm
                         , VC.withAggregation
                         , VC.chartMeasureOne] ]
      [ measureLabel
      , primaryOptions cell r (_pieConfiguration .. _firstMeasures)
      , aggSelect cell r (_pieConfiguration.._firstMeasures) (_pieConfiguration.._firstAggregation)
      ]
    , H.form [ A.classes [ B.colXs4
                         , VC.chartConfigureForm
                         , VC.chartSeriesOne] ]
      [ seriesLabel
      , secondaryOptions cell r (_pieConfiguration .. _firstSeries)
      ]
    ]
  , row
    [ H.form [ A.classes [ B.colXs4
                         , B.colXsOffset8
                         , VC.chartConfigureForm
                         , VC.chartSeriesTwo] ]
      [ seriesLabel
      , secondaryOptions cell r (_pieConfiguration .. _secondSeries)
      ]
    ]
  ]
aggSelect :: forall e. Cell -> VizRec -> LensP VizRec JSelection -> LensP VizRec Aggregation -> HTML e
aggSelect cell r _blocker _agg =
  H.select [ A.classes [ B.formControl, VC.aggregation, B.btnPrimary ]
           , E.onInput (\s -> pure $ maybe empty (selectAgg cell _agg) $ str2aggregation s),
             A.disabled (null (r ^._blocker.._variants))
           ]
  (aggOpt (r ^._agg) <$> allAggregations)

aggOpt :: forall e. Aggregation -> Aggregation -> HTML e
aggOpt sel a =
  H.option [ A.value (aggregation2str a)
           , A.selected (sel == a)
           ] [ H.text (aggregation2str a) ]

option :: forall e. Maybe JCursor -> JCursor -> Int -> HTML e
option selected cursor ix =
  H.option [ A.selected (Just cursor == selected)
           , A.value (show ix) ]
  [ H.text (show cursor) ]

defaultOption :: forall e. Maybe JCursor ->  HTML e
defaultOption selected =
  H.option [ A.selected (selected == Nothing)
           , A.value "-1" ]
  [ H.text "Select axis source" ]

options :: forall e. (Int -> Boolean) ->
           (Int -> Boolean) ->
           Cell -> VizRec -> LensP VizRec JSelection -> HTML e
options disableWhen defaultWhen cell r _sel =
  H.select [ A.classes [ B.formControl ]
           , E.onInput (\ix -> pure $ updateR (byIx ix vars))
           , A.disabled (disableWhen $ length vars)
           ]

  (defOption <> (zipWith (option selected) vars (range 0 $ length vars)))
  where
  defOption :: Array (HTML e)
  defOption =
    if defaultWhen $ length vars
    then [defaultOption selected]
    else [ ]

  vars :: Array JCursor
  vars = r ^._sel.._variants

  selected :: Maybe JCursor
  selected = r ^._sel.._selection

  byIx :: String -> Array JCursor -> Maybe JCursor
  byIx ix xs = s2i ix >>= (xs !!)

  updateR :: Maybe JCursor -> I e
  updateR cursor =
    updateViz cell (r # _sel.._selection .~ cursor)

-- types don't check 0_o
-- primaryOptions :: forall e. Cell -> VizRec -> LensP VizRec JSelection -> HTML e
primaryOptions = options (< 2) (> 1)
-- secondaryOptions :: forall e. Cell -> VizRec -> LensP VizRec JSelection -> HTML e
secondaryOptions = options (< 1) (const true)


barConfiguration :: forall e. Cell -> VizRec -> Boolean -> HTML e
barConfiguration cell r visible =
  H.div [ A.classes $ [VC.barEditor] <> (if visible then [] else [B.hide]) ]
  [ row
    [ H.form [ A.classes [ B.colXs4, VC.chartConfigureForm, VC.chartCategory] ]
      [ categoryLabel
      , (primaryOptions cell r (_barConfiguration.._cats))
      ]
    , H.form [ A.classes [ B.colXs4
                         , VC.chartConfigureForm
                         , VC.withAggregation
                         , VC.chartMeasureOne] ]
      [ measureLabel
      , (primaryOptions cell r (_barConfiguration.._firstMeasures))
      , aggSelect cell r (_barConfiguration.._firstMeasures) (_barConfiguration.._firstAggregation)
      ]
    , H.form [ A.classes [ B.colXs4
                         , VC.chartConfigureForm
                         , VC.chartSeriesOne] ]
      [ seriesLabel
      , (secondaryOptions cell r (_barConfiguration.._firstSeries))
      ]
    ]
  , row
    [ H.form [ A.classes [ B.colXs4
                         , B.colXsOffset8
                         , VC.chartConfigureForm
                         , VC.chartSeriesTwo] ]
      [ seriesLabel
      , (secondaryOptions cell r (_barConfiguration.._secondSeries))
      ]
    ]
  ]

lineConfiguration :: forall e. Cell -> VizRec -> Boolean -> HTML e
lineConfiguration cell r visible =
  H.div [ A.classes $ [VC.lineEditor] <> (if visible then [] else [B.hide]) ]
  [ row
    [ H.form [ A.classes [ B.colXs4
                         , VC.chartConfigureForm
                         , VC.chartDimension] ]
      [ dimensionLabel
      , (primaryOptions cell r (_lineConfiguration.._dims))
      ]
    , H.form [ A.classes [ B.colXs4
                         , VC.chartConfigureForm
                         , VC.withAggregation
                         , VC.chartMeasureOne] ]
      [ measureLabel
      , (primaryOptions cell r (_lineConfiguration.._firstMeasures))
      , aggSelect cell r (_lineConfiguration.._firstMeasures) (_lineConfiguration.._firstAggregation)
      ]
    , H.form [ A.classes [ B.colXs4
                         , VC.chartConfigureForm
                         , VC.chartSeriesOne] ]
      [ seriesLabel
      , (secondaryOptions cell r (_lineConfiguration.._firstSeries))
      ]
    ]
  , row
    [ H.form [ A.classes [ B.colXs4
                         , B.colXsOffset4
                         , VC.chartConfigureForm
                         , VC.withAggregation
                         , VC.chartMeasureTwo] ]
      [ measureLabel
      , (secondaryOptions cell r (_lineConfiguration.._secondMeasures))
      , aggSelect cell r (_lineConfiguration.._secondMeasures) (_lineConfiguration.._secondAggregation)
      ]
    , H.form [ A.classes [ B.colXs4
                         , VC.chartConfigureForm
                         , VC.chartSeriesTwo] ]
      [ seriesLabel
      , (secondaryOptions cell r (_lineConfiguration.._secondSeries))
      ]
    ]
  ]

seriesLabel :: forall e. HTML e
seriesLabel = label "Series"

measureLabel :: forall e. HTML e
measureLabel = label "Measure"

categoryLabel :: forall e. HTML e
categoryLabel = label "Category"

dimensionLabel :: forall e. HTML e
dimensionLabel = label "Dimension"

label :: forall e. String -> HTML e
label str =
  H.label [ A.classes [ B.controlLabel ] ]
  [ H.text str ]


loading :: forall e. Array (HTML e)
loading =
  [H.div [ A.classes [ B.alert, B.alertInfo, VC.loadingMessage ]
         ]
   [ H.text "Loading"
   , H.img [ A.src "/img/blue-spin.gif" ] [ ]
   ]
  ]


errored :: forall e. String -> Array (HTML e)
errored message =
  [H.div [ A.classes [ B.alert, B.alertDanger ]
         , CSS.style do
              marginBottom $ px 12.0
         ]
   [ H.text message ]
  ]


vizOutput :: forall e. Cell -> Array (HTML e)
vizOutput state =
  case state ^. _err of
    "" -> [chart
           (show $ state ^._cellId)
           (fromMaybe zero (state ^? _viz.._chartHeight))
           (fromMaybe zero (state ^? _viz.._chartWidth))
          ]
    _ -> [ ]

chart' :: forall e i. Array (A.Attr (I e)) -> String -> Int -> Int -> HTML e
chart' attrs chartId h w =
  H.div (attrs <>
         [ dataEChartsId chartId
         , A.key ("echarts-key" <> chartId)
         , CSS.style (do height $ px $ toNumber h
                         width $ px $ toNumber w
                         position relative
                         left $ pct 50.0
                         marginLeft $ px $ -0.5 * (toNumber w)
                     )
         ]) [ ]

chart :: forall e. String -> Int -> Int -> HTML e
chart = chart' []


_viz :: TraversalP Cell VizRec
_viz = _content .. _Visualize

_err :: TraversalP Cell String
_err = _viz .. _error

