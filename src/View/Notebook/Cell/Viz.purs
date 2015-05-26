module View.Notebook.Cell.Viz (vizChoices, vizOutput) where

import Data.Maybe (Maybe(..), maybe)

import qualified Data.StrMap as M 
import Optic.Core ((^.), (..))
import Optic.Fold ((^?))
import Optic.Extended (TraversalP())
import Data.Array (range, zipWith, length, drop, take)
import Data.Argonaut.JCursor (JCursor())

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
import Model.Notebook.Cell.Viz (VizRec(), _error, _xs, _ys, _xCursor, _yCursor, ChartType(..))
import Controller.Notebook.Common (I())
import Controller.Notebook.Cell.Viz (handleXAxisSelected, handleYAxisSelected, handleChartType)
import View.Common (row)
import qualified View.Css as VC 
import View.Notebook.Common (HTML(), dataCellId, dataCellType, dataEChartsId)
import Data.Foreign.Class ()

vizChoices :: forall e. Cell -> HTML e
vizChoices cell = 
  case cell ^. _err of
    "" -> correct cell
    str -> errored str

correct :: forall e. Cell -> HTML e
correct cell =
  row 
  [ H.form [ A.classes [ B.colXs10 ] ]
    [ H.div [ A.classes [ B.formGroup ] ]
      [ H.label_ [ H.text "Category axis" ]
      , H.select  [ A.classes [ B.formControl ]
                  , E.onValueChanged (\ix -> pure $ handleXAxisSelected ix cell)
                  ] 
        (defaultOptionX:(zipWith option xs (range 0 (length xs))))
      ] 
    , H.div [ A.classes [ B.formGroup ] ]
      [ H.label_ [ H.text "Value axis " ]
        
      , H.select [ A.classes [ B.formControl ]
                 , E.onValueChanged (\ix -> pure $ handleYAxisSelected ix cell)
                 ]
        (defaultOptionY:(zipWith option ys (range 0 (length ys))))
      ]
    , H.div [ A.classes [ B.formGroup ] ]
      [ H.label [ A.classes [ B.radioInline ] ]
        [ H.input [ A.type_ "radio"
                  , A.value "pie"
                  , A.name "chart-type"
                  , A.selected true
                  , E.onClick (\_ -> pure $ handleChartType Pie cell)
                  ] [ ]
        , H.text "Pie"
        ]
      , H.label [ A.classes [ B.radioInline ] ]
        [ H.input [ A.type_ "radio"
                  , A.value "line"
                  , A.name "chart-type"
                  , E.onClick (\_ -> pure $ handleChartType Line cell)
                  ] [ ]
        , H.text "Line"
        ]
      , H.label [ A.classes [ B.radioInline ] ]
        [ H.input [ A.type_ "radio"
                  , A.value "bar"
                  , A.name "chart-type"
                  , E.onClick (\_ -> pure $ handleChartType Bar cell)
                  ] [ ]
        , H.text "Bar"
        ] 
      ] 
    ] 
  ] 
  where 
  xs = cell ^. _xxs
  ys = cell ^. _yys 

defaultOption :: forall e. String -> HTML e
defaultOption txt =
  H.option [ A.value "-1" ] [ H.text txt ]

defaultOptionX :: forall e. HTML e
defaultOptionX = defaultOption "Please select x-axis/category source"

defaultOptionY :: forall e. HTML e
defaultOptionY = defaultOption "Please select y-axis/value source"

option :: forall e. JCursor -> Number -> HTML e
option cursor number =
  H.option [ A.value (show number) ] [ H.text (show cursor) ]

errored :: forall e. String -> HTML e
errored message =
  H.div [ A.classes [ B.alert, B.alertDanger ]
        , CSS.style do
             marginBottom $ px 12
        ]
  [ H.text message ]
  

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

_xxs :: TraversalP Cell [JCursor]
_xxs = _viz .. _xs

_yys :: TraversalP Cell [JCursor]
_yys = _viz .._ys

_cursorX :: TraversalP Cell (Maybe JCursor)
_cursorX = _viz .. _xCursor

_cursorY :: TraversalP Cell (Maybe JCursor)
_cursorY = _viz .. _yCursor
