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

import ECharts.Options

import Model.Notebook.ECharts (Axis(..))
import Model.Notebook.Cell (Cell(), _cellId, _content, _Visualize)

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
  case cell ^. _err of
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
   (img <$> S.toList (r ^._availableChartTypes))
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
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
      [ measureLabel
      , (options cell r (_pieConfiguration.._firstMeasures))
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
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
      [ measureLabel
      , (options cell r (_barConfiguration.._firstMeasures))
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
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
      [ measureLabel
      , (options cell r (_lineConfiguration.._firstMeasures))
      ]
    , H.form [ A.classes [ B.colXs4, VC.chartConfigureForm ] ]
      [ seriesLabel
      , (options cell r (_lineConfiguration.._firstSeries))
      ]
    ]
  , row
    [ H.form [ A.classes [ B.colXs4, B.colXsOffset4, VC.chartConfigureForm ] ]
      [ measureLabel
      , (options cell r (_lineConfiguration.._secondMeasures))
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
    "" -> [chart ((show $ state ^._cellId)) 300 ]
    _ -> [ ]

chart' :: forall e i. [A.Attr (I e)] -> String -> Number -> HTML e
chart' attrs chartId h =
  H.div (attrs <>
         [ dataEChartsId chartId
         , A.key ("echarts-key" <> chartId)
         , CSS.style (height $ px h)
         ]) [ ]

chart :: forall e. String -> Number -> HTML e
chart = chart' []


_viz :: TraversalP Cell VizRec
_viz = _content .. _Visualize 

_err :: TraversalP Cell String
_err = _viz .. _error

