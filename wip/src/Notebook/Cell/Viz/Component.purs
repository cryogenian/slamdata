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

import Control.Apply ((*>))
import Control.Monad (when)
import Control.MonadPlus (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Css.Geometry (marginBottom)
import Css.Size (px)
import Data.Argonaut (JCursor())
import Data.Array (length, null, cons, index)
import Data.Foldable (foldl)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, right, left)
import Data.Lens ((.~), view, preview)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, fromMaybe)
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
import Model.ChartConfiguration (ChartConfiguration(..), depends, dependsOnArr)
import Model.ChartOptions (buildOptions)
import Model.ChartType (ChartType(..), isPie)
import Model.Port as P
import Model.Resource as R
import Model.Select
  (Select(), autoSelect, newSelect, (<->), ifSelected, trySelect', _value)
import Notebook.Cell.Common.EvalQuery
  (CellEvalQuery(..), CellEvalT(), runCellEvalT)
import Notebook.Cell.Component (CellStateP(), CellQueryP(), makeEditorCellComponent, makeQueryPrism', _VizState, _VizQuery)
import Notebook.Cell.Viz.Component.Query
import Notebook.Cell.Viz.Component.State
import Notebook.Cell.Viz.Form.Component (formComponent)
import Notebook.Cell.Viz.Form.Component as Form
import Notebook.Common (Slam(), liftAff'')
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
  , sample: M.empty
  , records: []
  , needToUpdate: true
  , axisLabelFontSize: 12
  , axisLabelAngle: 30
  }

vizComponent :: Component CellStateP CellQueryP Slam
vizComponent = makeEditorCellComponent
  { name: cellName Viz
  , glyph: cellGlyph Viz
  , component: parentComponent' render eval peek
  , initialState: installedState initialState
  , _State: _VizState
  , _Query: makeQueryPrism' _VizQuery
  }

render :: VizState -> VizHTML
render state =
  H.div_ $
  [ renderLoading $ not state.loading
  , renderEmpty $ state.loading || (not $ Set.isEmpty state.availableChartTypes)
  , renderForm state
  ]

renderLoading :: Boolean -> VizHTML
renderLoading hidden =
  H.div [ P.classes $ [ B.alert
                      , B.alertInfo
                      , Rc.loadingMessage
                      ]
                     <> (guard hidden $> B.hide)
        ]
  [ H.text "Loading"
  , H.img [ P.src "/img/blue-spin.gif" ]
  ]

renderEmpty :: Boolean -> VizHTML
renderEmpty hidden =
  H.div [ P.classes $ [ B.alert
                      , B.alertDanger
                      ]
                     <> (guard hidden $> B.hide)
        , CSS.style $ marginBottom $ px 12.0
        ]
  [ H.text "There is no available chart for this dataset" ]

renderForm :: VizState -> VizHTML
renderForm state =
  H.div [ P.classes $ [ Rc.vizCellEditor ]
                    <> (guard hidden $> B.hide)
        ]
  [ renderChartTypeSelector state
  , renderChartConfiguration state
  ]
  where
  hidden :: Boolean
  hidden =
       Set.isEmpty state.availableChartTypes
    || state.loading


renderChartTypeSelector :: VizState -> VizHTML
renderChartTypeSelector state =
  H.div [ P.classes [ Rc.vizChartTypeSelector ] ]
  $ foldl (foldFn state.chartType) empty state.availableChartTypes
  where
  foldFn :: ChartType -> Array VizHTML -> ChartType -> Array VizHTML
  foldFn selected accum current =
    flip cons accum $   H.img [ P.src $ src current
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
  [ chartInput Rc.chartSizeParam "Height"
      (_.height >>> showIfNeqZero) SetHeight false
  , chartInput Rc.chartSizeParam "Width"
      (_.width >>> showIfNeqZero) SetWidth false
  , chartInput Rc.axisLabelParam "Axis label angle"
      (_.axisLabelAngle >>> show) RotateAxisLabel (isPie state.chartType)
  , chartInput Rc.axisLabelParam "Axis font size"
      (_.axisLabelFontSize >>> show) SetAxisFontSize (isPie state.chartType)
  ]
  where
  chartInput
    :: H.ClassName
    -> String
    -> (VizState -> String)
    -> (Int -> Unit -> VizQuery Unit)
    -> Boolean -> VizHTML
  chartInput cls labelText valueFromState queryCtor isHidden =
    H.form [ P.classes $ [ B.colXs3, cls ]
                      <> (guard isHidden $> B.hide)
           , Cp.nonSubmit
           ]
    [ label labelText
    , H.input [ P.classes [ B.formControl ]
              , P.value $ valueFromState state
              , Cp.ariaLabel labelText
              , Cp.mbValueInput (pure
                                 <<< map (right <<< flip queryCtor unit)
                                 <<< stringToInt'
                                )
              ]
    ]

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
vizEval q = do
  modify $ _needToUpdate .~ false
  case q of
    SetHeight h next ->
      modify (_height .~ h) *> configure $> next
    SetWidth w next ->
      modify (_width .~ w) *> configure $> next
    SetChartType ct next ->
      modify (_chartType .~ ct) *> configure $> next
    SetAvailableChartTypes ts next ->
      modify (_availableChartTypes .~ ts) *> configure $> next
    RotateAxisLabel angle next ->
      modify (_axisLabelAngle .~ angle) *> configure $> next
    SetAxisFontSize size next ->
      modify (_axisLabelFontSize .~ size) *> configure $> next

cellEval :: Natural CellEvalQuery VizDSL
cellEval (EvalCell info continue) = do
  needToUpdate <- gets _.needToUpdate
  map continue $ withLoading $ runCellEvalT do
    when needToUpdate do
      r <- maybe (throwError "Incorrect port in visual builder cell") pure
           $ info.inputPort >>= preview P._Resource
      lift $ updateForms r
      records <- lift $ liftAff'' $ Api.all r
      when (length records > 10000)
        $ throwError
        $  "Maximum record count available for visualization -- 10000, "
        <> "please consider using 'limit' or 'group by' in your request"
      lift $ modify $ _records .~ records
    responsePort
  where
  withLoading action = do
    modify $ _loading .~ true
    a <- action
    modify $ _loading .~ false
    modify $ _needToUpdate .~ true
    pure a
cellEval (NotifyRunCell next) = pure next

responsePort :: CellEvalT VizDSL P.Port
responsePort = do
  state <- lift $ get
  mbConf <- lift $ query state.chartType $ left $ request Form.GetConfiguration
  conf <- maybe (throwError "Form state has not been set in responsePort")
          pure mbConf
  pure
    $ P.ChartOptions
    { options: buildOptions state conf
    , width: state.width
    , height: state.height
    }

updateForms :: R.Resource -> VizDSL Unit
updateForms file = do
  jarr <- liftAff'' $ Api.sample file 0 20
  if null jarr
    then
    modify $ _availableChartTypes .~ Set.empty
    else do
    modify (_sample .~ analyzeJArray jarr)
    configure

type AxisAccum =
 { category :: Array JCursor
 , value :: Array JCursor,
   time :: Array JCursor
 }

configure :: VizDSL Unit
configure = void do
  axises <- getAxises
  pieConf <- getOrInitial Pie
  setConfFor Pie $ pieBarConfiguration axises pieConf
  lineConf <- getOrInitial Line
  setConfFor Line $ lineConfiguration axises lineConf
  barConf <- getOrInitial Bar
  setConfFor Bar $ pieBarConfiguration axises barConf
  modify (_availableChartTypes .~ available axises)
  where
  getOrInitial :: ChartType -> VizDSL ChartConfiguration
  getOrInitial ty =
    map (fromMaybe Form.initialState)
    $ query ty $ left (request Form.GetConfiguration)

  setConfFor :: ChartType -> ChartConfiguration -> VizDSL Unit
  setConfFor ty conf =
    void $ query ty $ left $ action $ Form.SetConfiguration conf

  available :: AxisAccum -> Set.Set ChartType
  available axises =
    foldMap Set.singleton
    $ if null axises.value
      then []
      else if not $ null axises.category
           then [Pie, Bar, Line]
           else if null axises.time
                then []
                else [Line]

  getAxises :: VizDSL AxisAccum
  getAxises = do
    sample <- gets _.sample
    pure $ foldl axisFolder {category: [], value: [], time: [] } $ M.toList sample

  axisFolder :: AxisAccum -> Tuple JCursor Axis -> AxisAccum
  axisFolder accum (Tuple cursor axis)
    | Ax.isCatAxis axis = accum { category = cons cursor accum.category }
    | Ax.isValAxis axis = accum { value = cons cursor accum.value }
    | Ax.isTimeAxis axis = accum {time = cons cursor accum.time }
    | otherwise = accum


  setPreviousValueFrom
    :: forall a. (Eq a) => Maybe (Select a) -> Select a -> Select a
  setPreviousValueFrom mbSel target  =
    (maybe id trySelect' $ mbSel >>= view _value) $ target

  pieBarConfiguration :: AxisAccum -> ChartConfiguration -> ChartConfiguration
  pieBarConfiguration axises (ChartConfiguration current) =
    let categories =
          setPreviousValueFrom (index current.series 0)
          $ autoSelect $ newSelect $ axises.category
        measures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends categories axises.value
        firstSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [categories] $ axises.category <-> categories
        secondSeries =
          setPreviousValueFrom (index current.series 2)
          $ newSelect $ ifSelected [categories, firstSeries]
          $ axises.category <-> categories <-> firstSeries
        aggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
    in ChartConfiguration { series: [categories, firstSeries, secondSeries]
                          , dimensions: []
                          , measures: [measures]
                          , aggregations: [aggregation]}

  lineConfiguration :: AxisAccum -> ChartConfiguration -> ChartConfiguration
  lineConfiguration axises (ChartConfiguration current) =
    let dimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ dependsOnArr axises.value
          -- This is redundant, I've put it here to notify
          -- that this behaviour differs from pieBar and can be changed.
          $ (axises.category <> axises.time <> axises.value)
        firstMeasures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends dimensions
          $ axises.value <-> dimensions
        secondMeasures =
          setPreviousValueFrom (index current.measures 1)
          $ newSelect $ ifSelected [firstMeasures]
          $ depends dimensions
          $ axises.value <-> firstMeasures <-> dimensions
        firstSeries =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [dimensions] $ axises.category <-> dimensions
        secondSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [dimensions, firstSeries]
          $ axises.category <-> dimensions <-> firstSeries
    in ChartConfiguration { series: [firstSeries, secondSeries]
                          , dimensions: [dimensions]
                          , measures: [firstMeasures, secondMeasures]
                          , aggregations: [aggregationSelect, aggregationSelect]}

peek :: forall a. ChildF ChartType Form.QueryP a -> VizDSL Unit
peek (ChildF chartType q) = do
  modify $ _needToUpdate .~ false
  configure
