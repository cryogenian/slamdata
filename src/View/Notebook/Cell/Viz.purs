module View.Notebook.Cell.Viz (vizChoices, vizOutput) where

import Control.Plus (empty)
import Data.Maybe (Maybe(..), maybe, fromMaybe)

import qualified Data.StrMap as M 
import Optic.Core ((^.), (..), LensP(), (.~))
import Optic.Fold ((^?))
import Optic.Extended (TraversalP())
import Data.Array (range, zipWith, length, drop, take, (!!), null)
import Data.Argonaut.JCursor (JCursor())
import qualified Data.Set as S 

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.CSS as CSS

import Css.Size
import Css.Geometry
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

correct :: forall e. Cell -> [HTML e]
correct cell =
  maybe [ ] go $ (cell ^? _content.._Visualize)
  where
  go :: VizRec -> [HTML e]
  go r = 
    [H.div [ A.classes [ VC.vizCellEditor ] ] $
     (chartTypeSelector cell r) <>
     (chartConfiguration cell r)
    ]

chartTypeSelector :: forall e. Cell -> VizRec -> [HTML e]
chartTypeSelector cell r =
  [H.div [ A.classes [ VC.vizChartTypeSelector ] ]
   (img <$> S.toList  (r ^._availableChartTypes))
  ] 
  where
  img :: ChartType -> HTML e
  img ct =
    H.img [ A.src (src ct)
          , A.classes (if (r ^._chartType) == ct
                       then [B.active]
                       else [])
          , E.onClick (\_ -> pure $ selectChartType cell ct)
          ]
    [ ]
  src :: ChartType -> String
  src Pie = "img/pie.svg"
  src Line = "img/line.svg"
  src Bar = "img/bar.svg"

chartConfiguration :: forall e. Cell -> VizRec -> [HTML e]
chartConfiguration cell r =
  [H.div [ A.classes [ VC.vizChartConfiguration ] ]
   [ pieConfiguration cell r (ct == Pie) 
   , lineConfiguration cell r (ct == Line) 
   , barConfiguration cell r (ct == Bar)
   , row
     [ H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
       [ label "Height"
       , H.input [ A.value (if 0 == (r ^._chartHeight)
                            then ""
                            else show (r ^._chartHeight))
                 , A.classes [ B.formControl ]
                 , A.type_ "number"
                 , E.onInput (\v -> pure $ maybe empty (setChartHeight cell) $ s2i v)
                 ] [ ]
       ]
     , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
       [ label "Width"
       , H.input [ A.value (if 0 == (r ^._chartWidth)
                            then ""
                            else show (r ^._chartWidth))
                 , A.classes [ B.formControl ]
                 , A.type_ "number"
                 , E.onInput (\v -> pure $ maybe empty (setChartWidth cell) $ s2i v)
                 ] [ ]
       ]
     ]
   ] 
  ]
  where ct = r ^._chartType

pieConfiguration :: forall e. Cell -> VizRec -> Boolean ->  HTML e
pieConfiguration cell r visible =
  H.div [ A.classes (if visible then [] else [B.hide]) ]
  [ row
    [ H.form [ A.classes [ B.colXs4, VC.chartConfigureForm] ]
      [ categoryLabel
      , (options cell r (_pieConfiguration.._cats))
      ]
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm, VC.withAggregation ] ]
      [ measureLabel
      , (options cell r (_pieConfiguration.._firstMeasures))
      , aggSelect cell r (_pieConfiguration.._firstMeasures) (_pieConfiguration.._firstAggregation)
      ]
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
      [ seriesLabel
      , (options cell r (_pieConfiguration.._firstSeries))
      ]
    ]
  , row
    [ H.form [ A.classes [ B.colXs4, B.colXsOffset8, VC.chartConfigureForm ] ]
      [ seriesLabel
      , (options cell r (_pieConfiguration.._secondSeries))
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

option :: forall e. Maybe JCursor -> JCursor -> Number -> HTML e
option selected cursor ix =
  H.option [ A.selected (Just cursor == selected) 
           , A.value (show ix) ]
  [ H.text (show cursor) ]

defaultOption :: forall e. Maybe JCursor ->  HTML e
defaultOption selected =
  H.option [ A.selected (selected == Nothing)
           , A.value "-1" ]
  [ H.text "Select axis source" ]

options :: forall e. Cell -> VizRec -> LensP VizRec JSelection -> HTML e
options cell r _sel =
  H.select [ A.classes [ B.formControl ]
           , E.onInput (\ix -> pure $ updateR (byIx ix vars))
           , A.disabled (length vars < 1)
           ] 
  ((defaultOption selected):(zipWith (option selected) vars (range 0 (length vars))))
  where
  vars :: [JCursor]
  vars = r ^._sel.._variants

  selected :: Maybe JCursor
  selected = r ^._sel.._selection

  byIx :: String -> [JCursor] -> Maybe JCursor
  byIx ix xs = s2i ix >>= (xs !!)

  updateR :: Maybe JCursor -> I e
  updateR cursor =
    updateViz cell (r # _sel.._selection .~ cursor)

  
barConfiguration :: forall e. Cell -> VizRec -> Boolean -> HTML e
barConfiguration cell r visible = 
  H.div [ A.classes (if visible then [] else [B.hide]) ]
  [ row
    [ H.form [ A.classes [ B.colXs4, VC.chartConfigureForm] ]
      [ categoryLabel
      , (options cell r (_barConfiguration.._cats))
      ]
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm, VC.withAggregation ] ]
      [ measureLabel
      , (options cell r (_barConfiguration.._firstMeasures))
      , aggSelect cell r (_barConfiguration.._firstMeasures) (_barConfiguration.._firstAggregation)
      ]
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
      [ seriesLabel
      , (options cell r (_barConfiguration.._firstSeries))
      ]
    ]
  , row
    [ H.form [ A.classes [ B.colXs4, B.colXsOffset8, VC.chartConfigureForm ] ]
      [ seriesLabel
      , (options cell r (_barConfiguration.._secondSeries))
      ]
    ] 
  ]
 
lineConfiguration :: forall e. Cell -> VizRec -> Boolean -> HTML e
lineConfiguration cell r visible =
  H.div [ A.classes (if visible then [] else [B.hide]) ]
  [ row
    [ H.form [ A.classes [ B.colXs4, VC.chartConfigureForm] ]
      [ dimensionLabel
      , (options cell r (_lineConfiguration.._dims))
      ]
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm, VC.withAggregation ] ]
      [ measureLabel
      , (options cell r (_lineConfiguration.._firstMeasures))
      , aggSelect cell r (_lineConfiguration.._firstMeasures) (_lineConfiguration.._firstAggregation)
      ]
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
      [ seriesLabel
      , (options cell r (_lineConfiguration.._firstSeries))
      ]
    ]
  , row
    [ H.form [ A.classes [ B.colXs4, B.colXsOffset4, VC.chartConfigureForm, VC.withAggregation ] ]
      [ measureLabel
      , (options cell r (_lineConfiguration.._secondMeasures))
      , aggSelect cell r (_lineConfiguration.._secondMeasures) (_lineConfiguration.._secondAggregation)
      ]
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
      [ seriesLabel
      , (options cell r (_lineConfiguration.._secondSeries))
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


loading :: forall e. [HTML e]
loading =
  [H.div [ A.classes [ B.alert, B.alertInfo, VC.loadingMessage ]
         ]
   [ H.text "Loading"
   , H.img [ A.src "/img/blue-spin.svg" ] [ ]
   ]
  ] 

  
errored :: forall e. String -> [HTML e]
errored message =
  [H.div [ A.classes [ B.alert, B.alertDanger ]
         , CSS.style do
              marginBottom $ px 12
         ]
   [ H.text message ]
  ]
  

vizOutput :: forall e. Cell -> [ HTML e ]
vizOutput state =
  case state ^. _err of
    "" -> [chart
           (show $ state ^._cellId)
           (fromMaybe 0 (state ^? _viz.._chartHeight))
           (fromMaybe 0 (state ^? _viz.._chartWidth))
          ]
    _ -> [ ]

chart' :: forall e i. [A.Attr (I e)] -> String -> Number -> Number -> HTML e
chart' attrs chartId h w =
  H.div (attrs <>
         [ dataEChartsId chartId
         , A.key ("echarts-key" <> chartId)
         , CSS.style (do height $ px h
                         width $ px w
                         position relative
                         left $ pct 50
                         marginLeft $ px $ -0.5 * w
                     )
         ]) [ ]

chart :: forall e. String -> Number -> Number -> HTML e
chart = chart' []


_viz :: TraversalP Cell VizRec
_viz = _content .. _Visualize 

_err :: TraversalP Cell String
_err = _viz .. _error

