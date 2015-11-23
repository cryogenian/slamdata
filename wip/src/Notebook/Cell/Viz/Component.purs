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

module Notebook.Cell.Viz.Component where

import Prelude

import Control.MonadPlus (guard)
import Control.Plus (empty)
import Css.Geometry (marginBottom)
import Css.Size (px)
import Data.Argonaut (JCursor())
import Data.Array (snoc, length, singleton, null, cons)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, right, left)
import Data.Lens ((.~))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Halogen
import Halogen.CustomProps.Indexed as Cp
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B
import Model.Aggregation (aggregationSelect)
import Model.CellType (CellType(Viz), cellName, cellGlyph)
import Model.ChartAxis (analyzeJArray, Axis())
import Model.ChartAxis as Ax
import Model.ChartConfiguration (ChartConfiguration(..), depends)
import Model.ChartOptions (buildOptions)
import Model.ChartType (ChartType(..))
import Model.Port as P
import Model.Resource as R
import Model.Select (autoSelect, newSelect, (<->), ifSelected)
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalResult())
import Notebook.Cell.Component (CellStateP(), CellQueryP(), makeEditorCellComponent, makeQueryPrism', _VizState, _VizQuery)
import Notebook.Cell.Viz.Component.Query
import Notebook.Cell.Viz.Component.State
import Notebook.Cell.Viz.Form.Component (formComponent)
import Notebook.Cell.Viz.Form.Component as Form
import Notebook.Common (Slam(), forceRerender', liftAff'')
import Quasar.Aff as Api
import Render.Common (row)
import Render.CssClasses as Rc
import Utils (stringToInt)


type Query = Coproduct CellEvalQuery VizQuery

type VizHTML = ParentHTML Form.StateP Query Form.QueryP Slam ChartType
type VizDSL = ParentDSL VizState Form.StateP Query Form.QueryP Slam ChartType

import Data.Foldable (foldMap)

initialState :: VizState
initialState =
  { width: 600
  , height: 400
  , chartType: Pie
  , availableChartTypes: Set.empty
  , loading: true
  }

vizComponent :: Component CellStateP CellQueryP Slam
vizComponent = makeEditorCellComponent
  { name: cellName Viz
  , glyph: cellGlyph Viz
  , component: parentComponent render eval
  , initialState: installedState initialState
  , _State: _VizState
  , _Query: makeQueryPrism' _VizQuery
  }

render :: VizState -> VizHTML
render state
--  | state.loading = renderLoading
--  | Set.isEmpty state.availableChartTypes = renderEmpty
  | otherwise = renderForm state

renderLoading :: VizHTML
renderLoading =
  H.div [ P.classes [ B.alert
                    , B.alertInfo
                    , Rc.loadingMessage
                    ]
        ]
  [ H.text "Loading"
  , H.img [ P.src "/img/blue-spin.gif" ]
  ]

renderEmpty :: VizHTML
renderEmpty =
  H.div [ P.classes [ B.alert
                    , B.alertDanger
                    ]
        , CSS.style $ marginBottom $ px 12.0
        ]
  [ H.text "There is no available chart for this dataset" ]

renderForm :: VizState -> VizHTML
renderForm state =
  traceAny state \_ ->
  H.div [ P.classes [ Rc.vizCellEditor ] ]
  [ renderChartTypeSelector state
  , renderChartConfiguration state
  ]

renderChartTypeSelector :: VizState -> VizHTML
renderChartTypeSelector state =
  H.div [ P.classes [ Rc.vizChartTypeSelector ] ]
  $ foldl (foldFn state.chartType) empty state.availableChartTypes
  where
  foldFn :: ChartType -> Array VizHTML -> ChartType -> Array VizHTML
  foldFn selected accum current =
    snoc accum $ H.img [ P.src $ src current
                       , P.classes $ [ cls state.chartType ]
                         <> (guard (selected == current) $> B.active)
                       , E.onClick (E.input_ (right <<< SetChartType current))
                       ]

  src :: ChartType -> String
  src Pie = "img/pie.svg"
  src Line = "img/line.svg"
  src Bar = "img/bar.svg"

  cls :: ChartType -> H.ClassName
  cls Pie = Rc.pieChartIcon
  cls Line = Rc.lineChartIcon
  cls Bar = Rc.barChartIcon


renderChartConfiguration :: VizState -> VizHTML
renderChartConfiguration state =
  H.div [ P.classes [ Rc.vizChartConfiguration ] ]
  [ renderTab Pie
  , renderTab Line
  , renderTab Bar
  , renderDimensions state
  ]
  where
  renderTab :: ChartType -> VizHTML
  renderTab ty =
    showIf (state.chartType == ty)
    [ H.slot ty \_ -> { component: formComponent
                      , initialState: installedState Form.initialState
                      }
    ]

  showIf :: Boolean -> Array VizHTML -> VizHTML
  showIf ok content = H.div [ P.classes $ (guard (not ok) $> B.hide) ] content


renderDimensions :: VizState -> VizHTML
renderDimensions state =
  row
  [ H.form [ P.classes [ B.colXs4, Rc.chartConfigureForm ] ]
    [ label "Height"
    , H.input [ P.value $ showIfNeqZero state.height
              , Cp.mbValueInput (pure
                                 <<< map (right <<< flip SetHeight unit)
                                 <<< stringToInt')
              ]

    ]
  , H.form [ P.classes [ B.colXs4, Rc.chartConfigureForm ] ]
    [ label "Width"
    , H.input [ P.value $ showIfNeqZero state.width
              , Cp.mbValueInput (pure
                                 <<< map (right <<< flip SetWidth unit)
                                 <<< stringToInt')
              ]
    ]
  ]
  where
  label :: String -> VizHTML
  label str = H.label [ P.classes [ B.controlLabel ] ] [ H.text str ]

  showIfNeqZero :: forall a. (Eq a, Show a, Semiring a) => a -> String
  showIfNeqZero a = if zero == a then "" else show a

  stringToInt' :: String -> Maybe Int
  stringToInt' s = if s == "" then Just 0 else stringToInt s

-- Note: need to put running to state
eval :: Natural Query VizDSL
eval = coproduct cellEval vizEval

vizEval :: Natural VizQuery VizDSL
vizEval (SetHeight h next) =
  modify (_height .~ h) $> next
vizEval (SetWidth w next) =
  modify (_width .~ w) $> next
vizEval (SetChartType ct next) =
  modify (_chartType .~ ct) $> next
vizEval (SetAvailableChartTypes ts next) =
  modify (_availableChartTypes .~ ts) $> next


import Debug.Trace
cellEval :: Natural CellEvalQuery VizDSL
cellEval (EvalCell info continue) = do
  -- TODO: CellEvalT
--  modify (_loading .~ true)
  traceAnyA "!!!"
  forceRerender'
  map continue case info.inputPort of
    Just (P.Resource r) -> do
      traceAnyA "TROLOLO"
      state <- get
      traceAnyA state
      traceAnyA "@"
      -- Form configuration hasn't been set in autorun
      mbConf <- query state.chartType $ left $ request Form.GetConfiguration
      traceAnyA "#"
      forceRerender'
      traceAnyA mbConf
      traceAnyA "$"
      case mbConf of
        Nothing -> do
          traceAnyA "EMpty"
          emptyResponse
        Just conf -> do
          traceAnyA "Just conf"
          jarr <- liftAff'' $ Api.sample r 0 20
          traceAnyA jarr
          if null jarr
            then do
            modify $ _availableChartTypes .~ Set.empty
            emptyResponse
            else do
            records <- liftAff'' $ Api.all r
            if length records > 10000
              then do
              modify (_availableChartTypes .~ Set.empty)
              pure { output: Nothing
                   , messages: singleton $ errorTooMuch
                   }
              else do
              forceRerender'
              mbConf' <- query state.chartType $ left
                         $ request Form.GetConfiguration
              case mbConf' of
                Nothing -> emptyResponse
                Just conf' ->
                  modify (_loading .~ false)
                  $> { output: Just $ P.ChartOptions $
                              { options: buildOptions state.chartType records conf'
                              , width: state.width
                              , height: state.height
                              }
                     , messages: [] }
            emptyResponse
    _ -> emptyResponse
  where
  emptyResponse :: VizDSL CellEvalResult
  emptyResponse = modify (_loading .~ false) $> { output: Nothing, messages: [] }

  errorTooMuch :: Either String String
  errorTooMuch = Left
                 $  "Maximum record count available for visualization -- 10000, "
                 <> "please consider to use 'limit' or 'group by' in your request"
cellEval (NotifyRunCell next) = pure next

updateForms :: R.Resource -> VizDSL Unit
updateForms file = do
  jarr <- liftAff'' $ Api.sample file 0 20
  if null jarr
    then
    -- Here I want to send notification, with error and disable play button
    modify $ _availableChartTypes .~ Set.empty
    else
    let sample = analyzeJArray jarr
    in configure sample

type AxisAccum =
 { category :: Array JCursor
 , value :: Array JCursor,
   time :: Array JCursor
 }
configure :: M.Map JCursor Axis -> VizDSL Unit
configure sample = void do
  query Pie $ left $ action $ Form.SetConfiguration pieBarConfiguration
  forceRerender'
  query Line $ left $ action $ Form.SetConfiguration lineConfiguration
  forceRerender'
  query Bar $ left $ action $ Form.SetConfiguration pieBarConfiguration
  forceRerender'
  modify (_availableChartTypes .~ available)
  where
  available :: Set.Set ChartType
  available =
    foldMap Set.singleton
    $ if null axises.value
      then []
      else if not $ null axises.category
           then [Pie, Bar, Line]
           else if null axises.time
                then []
                else [Line]

  axises :: AxisAccum
  axises =
    foldl axisFolder {category: [], value: [], time: [] } $ M.toList sample

  axisFolder :: AxisAccum -> Tuple JCursor Axis -> AxisAccum
  axisFolder accum (Tuple cursor axis)
    | Ax.isCatAxis axis = accum { category = cons cursor accum.category }
    | Ax.isValAxis axis = accum { value = cons cursor accum.value }
    | Ax.isTimeAxis axis = accum {time = cons cursor accum.time }
    | otherwise = accum

  pieBarConfiguration :: ChartConfiguration
  pieBarConfiguration =
    let categories =
          autoSelect $ newSelect $ axises.category
        measures =
          autoSelect $ newSelect $ depends categories axises.value
        firstSeries =
          newSelect $ ifSelected [categories] $ axises.category <-> categories
        secondSeries =
          newSelect $ ifSelected [categories, firstSeries]
          $ axises.category <-> categories <-> firstSeries
        aggregations =
          newSelect
    in ChartConfiguration { series: [categories, firstSeries, secondSeries]
                          , dimensions: []
                          , measures: [measures]
                          , aggregations: [aggregationSelect]}

  lineConfiguration :: ChartConfiguration
  lineConfiguration =
    let dimensions =
          autoSelect $ newSelect
          -- This is redundant, I've put it here to notify
          -- that this behaviour differs from pieBar and can be changed.
          $ (axises.category <> axises.time <> axises.value)
        firstMeasures =
          autoSelect $ newSelect $ depends dimensions axises.value
        secondMeasures =
          newSelect $ depends dimensions $ axises.value <-> firstMeasures
        firstSeries =
          newSelect $ ifSelected [dimensions] $ axises.category <-> dimensions
        secondSeries =
          newSelect $ ifSelected [dimensions, firstSeries]
          $ axises.category <-> dimensions <-> firstSeries
    in ChartConfiguration { series: [firstSeries, secondSeries]
                          , dimensions: [dimensions]
                          , measures: [firstMeasures, secondMeasures]
                          , aggregations: [aggregationSelect, aggregationSelect]}
