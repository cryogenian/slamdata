module View.Notebook.Cell.Viz (vizChoices, vizOutput) where

import Data.Maybe (Maybe(..), maybe, fromMaybe)

import qualified Data.StrMap as M 
import Optic.Core ((^.), (..))
import Optic.Fold ((^?))
import Optic.Extended (TraversalP())
import Data.Array (range, zipWith, length, drop, take)
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
import View.Common (row)
import qualified View.Css as VC 
import View.Notebook.Common (HTML(), dataCellId, dataCellType, dataEChartsId)
import Data.Foreign.Class ()
import Utils.Halide (max, min, step)

-- to many imports from it to make it explicit
import Model.Notebook.Cell.Viz
import Controller.Notebook.Cell.Viz

vizChoices :: forall e. Cell -> HTML e
vizChoices cell = 
  case cell ^. _err of
    "" -> correct cell
    str -> errored str

correct :: forall e. Cell -> HTML e
correct cell =
  row 
  [ H.form [ A.classes [ VC.chartConfigureForm ] ]
    ([ H.div [ A.classes [ B.formGroup ] ]
      (maybe [] (\s -> chartOption cell <$> S.toList s) 
       (cell ^? _content.._Visualize.._availableChartTypes))
     ] <>
     (maybe [] (chartConfiguration cell)
      (cell ^? _content.._Visualize.._chartType)))
  ] 
  
chartConfiguration :: forall e. Cell -> ChartType -> [HTML e]
chartConfiguration cell chartType = 
  case chartType of
    Pie -> pieConfiguration cell
    Line -> lineConfiguration cell
    Bar -> barConfiguration cell

sourceSelect :: forall e. String -> String -> Cell -> [HTML e]
sourceSelect firstLabel secondLabel cell = 
  [ H.div [ A.classes [B.formGroup] ]
    [ H.label_ [ H.text firstLabel ]
    , H.select [ A.classes [ B.formControl ]
               , E.onInput (\ix -> pure $ handleXAxisSelected ix cell)
               ]
      ((defaultXOption cell) <> (xOptions cell))
    ]
  , H.div [ A.classes [ B.formGroup ] ]
    [ H.label_ [ H.text secondLabel ]
    , H.select [ A.classes [ B.formControl ]
               , E.onInput (\ix -> pure $ handleYAxisSelected ix cell)
               ]
      ((defaultYOption cell) <> (yOptions cell))
    ]
  ]

defaultOption :: forall e. String -> (VizRec -> Number) -> Cell -> [HTML e]
defaultOption txt ix cell =
  maybe [] go (cell ^? _content.._Visualize)
  where
  go :: VizRec -> [HTML e]
  go r =
    [ H.option [ A.value "-1"
               , A.selected (-1 == ix r)
               ]
      [ H.text txt]
    ]

defaultXOption :: forall e. Cell -> [HTML e]
defaultXOption = defaultOption "Please select x-axis/category source" ixx

defaultYOption :: forall e. Cell -> [HTML e]
defaultYOption = defaultOption "Please select y-axis/value source" ixy

xOptions :: forall e. Cell -> [HTML e]
xOptions cell =
  maybe [] go (cell ^? _content.._Visualize)
  where
  go :: VizRec -> [HTML e]
  go r = zipWith (optionX r) xs (range 0 (length xs))
    where
    xs :: [JCursor]
    xs = r ^. _xs
    
yOptions :: forall e. Cell -> [HTML e]
yOptions cell =
  maybe [] go (cell ^? _content.._Visualize)
  where
  go :: VizRec -> [HTML e]
  go r = zipWith (optionY r) ys (range 0 (length ys))
    where
    ys :: [JCursor]
    ys = r ^. _ys

optionX :: forall e. VizRec -> JCursor -> Number -> HTML e
optionX r cursor number =
  H.option [ A.value (show number)
           , A.selected (number == ixx r)
           ]
  [ H.text (show cursor) ]

optionY :: forall e. VizRec -> JCursor -> Number -> HTML e
optionY r cursor number = 
  H.option [ A.value (show number)
           , A.selected (number == ixy r)
           ]
  [ H.text (show cursor) ]


pieConfiguration :: forall e. Cell -> [HTML e]
pieConfiguration cell =
  (sourceSelect "Category axis" "Value axis" cell) <>
  [ H.div [ A.classes [ B.formGroup ] ]
    [ H.label_ [ H.text "Pie rose type" ] ]
  , H.div [ A.classes [ B.formGroup ] ]
    [ H.label [ A.classes [ B.radioInline ] ]
      [ H.input [ A.type_ "radio"
                , A.value "area"
                , A.name "pie-rose"
                , A.selected (Just "area" == currentRoseType) 
                , E.onClick (\_ -> pure $ setPieRose cell "area")
                ]  [ ]
      , H.text "Area"
      ]
    , H.label [ A.classes [ B.radioInline ] ]
      [ H.input [ A.type_ "radio"
                , A.value "radius"
                , A.name "pie-rose"
                , A.selected (Just "radius" == currentRoseType)
                , E.onClick (\_ -> pure $ setPieRose cell "radius")
                ] [ ]
      , H.text "Radius"
      ]
    , H.label [ A.classes [ B.radioInline ] ]
      [ H.input [ A.type_ "radio"
                , A.value "none"
                , A.name "pie-rose"
                , A.selected (Just "none" == currentRoseType ||
                              Nothing == currentRoseType)
                , E.onClick (\_ -> pure $ setPieRose cell "none")
                ] [ ]
      , H.text "None"
      ]
    ]
  , H.div [ A.classes [ B.formGroup ] ]
    [ H.label_ [ H.text "donut radius ratio" ]
    , H.input [ A.type_ "number"
              , A.classes [ B.formControl ]
              , A.value (show currentDonutRatio)
              , step 1
              , min 0
              , max 100
              , E.onInput (\v -> pure $ setDonutRatio cell v)  
              ] [ ]
    ]
  , H.div [ A.classes [ B.formGroup ] ]
    [ H.label_ [ H.text "minimal angle" ]
    , H.input [ A.type_ "number"
              , A.classes [ B.formControl ]
              , A.value (show currentMinAngle)
              , min 0
              , max 360
              , E.onInput (\v -> pure $ setMinimalAngle cell v)
              ] [ ]
    ] 
  ]
  where
  currentRoseType :: Maybe String
  currentRoseType = cell ^? _chartOpts.._roseType

  currentDonutRatio :: Number
  currentDonutRatio = fromMaybe 0 (cell ^? _chartOpts.._donutRatio)

  currentMinAngle :: Number
  currentMinAngle = fromMaybe 0 (cell ^? _chartOpts.._minimalAngle)
    

lineConfiguration :: forall e. Cell -> [HTML e]
lineConfiguration cell =
  (sourceSelect "X-axis" "Y-axis" cell) <>
  ([ H.div [ A.classes [ B.checkbox ] ]
    [ H.label_
      [ H.input [ A.type_ "checkbox"
                , A.checked currentSmooth
                , E.onChecked (\v -> pure $ toggleSmooth cell v) ] [ ]
      , H.text "smooth"
      ]
    ]
  , H.div [ A.classes [ B.formGroup ] ]
    [ H.label_ [ H.text "symbol size" ]
    , H.input [ A.type_ "number"
              , A.classes [ B.formControl ]
              , A.value (show currentSymbolSize)
              , min 0
              , max 20
              , step 1
              , E.onInput (\v -> pure $ setSymbolSize cell v)
              ]
      [ ]
    ]
  ]) <>
  (configureAxisPositions cell)
  where
  currentSmooth :: Boolean
  currentSmooth = fromMaybe false (cell ^? _chartOpts.._smooth)

  currentSymbolSize :: Number
  currentSymbolSize = fromMaybe 0 (cell ^? _chartOpts.._symbolSize)

barConfiguration :: forall e. Cell -> [HTML e]
barConfiguration cell =
  (sourceSelect "X-axis" "Y-axis" cell) <>
  ([ H.div [ A.classes [ B.formGroup ] ]
     [ H.label_ [ H.text "Bar gap" ]
     , H.input [ A.type_ "number"
               , A.classes [ B.formControl ]
               , A.value (show currentBarGap)
               , min 0
               , max 100
               , step 1
               , E.onInput (\v -> pure $ setBarGap cell v) ] [ ]
     ] 
   ]) <>
  (configureAxisPositions cell)
  where
  currentBarGap :: Number
  currentBarGap = fromMaybe 30 (cell ^? _chartOpts.._barGap)

configureAxisPositions :: forall e. Cell -> [HTML e]
configureAxisPositions cell =
  [ H.div [ A.classes [ B.formGroup ] ]
    [ H.label_ [ H.text "Y-axis position" ]
    ] 
  , H.div [ A.classes [ B.formGroup ] ] 
    [ H.label [ A.classes [ B.radioInline ] ]
      [ H.input [ A.type_ "radio"
                , A.value "left"
                , A.name "y-axis-position"
                , A.selected (currentYAxisPosition == "left")
                , E.onClick (\_ -> pure $ setYAxisPosition cell "left") ] [ ]
      , H.text "Left"
      ]
    , H.label [ A.classes [ B.radioInline ] ]
      [ H.input [ A.type_ "radio"
                , A.value "right"
                , A.name "y-axis-position"
                , A.selected (currentYAxisPosition == "right")
                , E.onClick (\_ -> pure $ setYAxisPosition cell "right") ] [ ]
      , H.text "Right"
      ]
    ]
  , H.div [ A.classes [ B.formGroup ] ]
    [ H.label_ [ H.text "X-axis position" ]
    ]
  , H.div [ A.classes [ B.formGroup ] ]
    [ H.label [ A.classes [ B.radioInline ] ]
      [ H.input [ A.type_ "radio"
                , A.value "bottom"
                , A.name "x-axis-position"
                , A.selected (currentXAxisPosition == "bottom")
                , E.onClick (\_ -> pure $ setXAxisPosition cell "bottom") ] [ ]
      , H.text "Bottom"
      ]
    , H.label [ A.classes [ B.radioInline ] ]
      [ H.input [ A.type_ "radio"
                , A.value "top"
                , A.name "x-axis-position"
                , A.selected (currentXAxisPosition == "top")
                , E.onClick (\_ -> pure $ setXAxisPosition cell "top") ] [ ]
      , H.text "Top"
      ]
    ]
  ]
  where
  currentXAxisPosition :: String
  currentXAxisPosition = fromMaybe "bottom" (cell ^? _chartOpts.._xAxisPosition)

  currentYAxisPosition :: String
  currentYAxisPosition = fromMaybe "top" (cell ^? _chartOpts.._yAxisPosition)

  
chartOption :: forall e. Cell -> ChartType -> HTML e 
chartOption cell chartType =
  H.label [ A.classes [ B.radioInline ] ]
  [ H.input [ A.type_ "radio"
            , A.value txt
            , A.name "chart-type"
            , E.onClick (\_ -> pure $ handleChartType chartType cell)
            ] [ ]
  , H.text txt
  ]
  where txt = chartType2str chartType



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

