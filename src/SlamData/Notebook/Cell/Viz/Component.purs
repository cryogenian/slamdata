{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook.Cell.Viz.Component where

import Prelude

import Control.Apply ((*>))
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (lift)
import Control.MonadPlus (guard)
import Control.Plus (empty)

import Data.Argonaut (JCursor())
import Data.Array (length, null, cons, index)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Functor (($>))
import Data.Functor.Coproduct (coproduct, right, left)
import Data.Int as Int
import Data.Lens ((.~), view, preview)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))

import CSS.Geometry (marginBottom)
import CSS.Size (px)

import Halogen
import Halogen.CustomProps as Cp
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Quasar.Aff as Api
import Quasar.Auth as Auth

import SlamData.FileSystem.Resource as R
import SlamData.Form.Select (Select(), autoSelect, newSelect, (<->), ifSelected, trySelect', _value)
import SlamData.Notebook.Cell.CellType (CellType(Viz), cellName, cellGlyph)
import SlamData.Notebook.Cell.Chart.Aggregation (aggregationSelect)
import SlamData.Notebook.Cell.Chart.Axis (analyzeJArray, Axis())
import SlamData.Notebook.Cell.Chart.Axis as Ax
import SlamData.Notebook.Cell.Chart.ChartConfiguration (ChartConfiguration(), depends, dependsOnArr)
import SlamData.Notebook.Cell.Chart.ChartOptions (buildOptions)
import SlamData.Notebook.Cell.Chart.ChartType (ChartType(..), isPie)
import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalT(), runCellEvalT, liftWithCanceler')
import SlamData.Notebook.Cell.Component (CellStateP(), CellQueryP(), makeEditorCellComponent, makeQueryPrism', _VizState, _VizQuery)
import SlamData.Notebook.Cell.Port as P
import SlamData.Notebook.Cell.Viz.Component.Query
import SlamData.Notebook.Cell.Viz.Component.State
import SlamData.Notebook.Cell.Viz.Form.Component (formComponent)
import SlamData.Notebook.Cell.Viz.Form.Component as Form
import SlamData.Notebook.Cell.Viz.Model as Model
import SlamData.Effects (Slam())
import SlamData.Render.Common (row)
import SlamData.Render.CSS as Rc

type VizHTML = ParentHTML Form.StateP QueryC Form.QueryP Slam ChartType
type VizDSL = ParentDSL State Form.StateP QueryC Form.QueryP Slam ChartType

import Data.Foldable (foldMap)

vizComponent :: Component CellStateP CellQueryP Slam
vizComponent = makeEditorCellComponent
  { name: cellName Viz
  , glyph: cellGlyph Viz
  , component: parentComponent' render eval peek
  , initialState: installedState initialState
  , _State: _VizState
  , _Query: makeQueryPrism' _VizQuery
  }

render :: State -> VizHTML
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

renderForm :: State -> VizHTML
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


renderChartTypeSelector :: State -> VizHTML
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


renderChartConfiguration :: State -> VizHTML
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


renderDimensions :: State -> VizHTML
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
    -> (State -> String)
    -> (Int -> Unit -> Query Unit)
    -> Boolean -> VizHTML
  chartInput cls labelText valueFromState queryCtor isHidden =
    H.form [ P.classes $ [ B.colXs3, cls ]
                      <> (guard isHidden $> B.hide)
           , Cp.nonSubmit
           ]
    [ label labelText
    , H.input [ P.classes [ B.formControl ]
              , P.value $ valueFromState state
              , ARIA.label labelText
              , Cp.mbValueInput (pure
                                 <<< map (right <<< flip queryCtor unit)
                                 <<< stringToInt
                                )
              ]
    ]

  label :: String -> VizHTML
  label str = H.label [ P.classes [ B.controlLabel ] ] [ H.text str ]

  showIfNeqZero :: forall a. (Eq a, Show a, Semiring a) => a -> String
  showIfNeqZero a = if zero == a then "" else show a

  stringToInt :: String -> Maybe Int
  stringToInt s = if s == "" then Just 0 else Int.fromString s

-- Note: need to put running to state
eval :: Natural QueryC VizDSL
eval = coproduct cellEval vizEval

vizEval :: Natural Query VizDSL
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
  map continue $ runCellEvalT do
    when needToUpdate $ withLoading do
      r <- maybe (throwError "Incorrect port in visual builder cell") pure
           $ info.inputPort >>= preview P._Resource
      lift $ updateForms r
      records <- lift $ liftWithCanceler' $ Auth.authed $ Api.all r
      when (length records > 10000)
        $ throwError
        $  "Maximum record count available for visualization -- 10000, "
        <> "please consider using 'limit' or 'group by' in your request"
      lift $ modify $ _records .~ records
    lift $ modify $ _needToUpdate .~ true
    responsePort
  where
  withLoading action = do
    lift $ modify $ _loading .~ true
    a <- action
    lift $ modify $ _loading .~ false
    pure a
cellEval (SetupCell _ next) = pure next
cellEval (NotifyRunCell next) = pure next
cellEval (Save k) = do
  st <- get
  config <- query st.chartType $ left $ request Form.GetConfiguration
  pure $ k $ Model.encode
    { width: st.width
    , height: st.height
    , chartType: st.chartType
    , chartConfig: fromMaybe Form.initialState config
    , axisLabelFontSize: st.axisLabelFontSize
    , axisLabelAngle: st.axisLabelAngle
    }
cellEval (Load json next) =
  case Model.decode json of
    Left err -> pure next
    Right model -> do
      let st = fromModel model
      set st
      query st.chartType $ left $ action $ Form.SetConfiguration model.chartConfig
      pure next
cellEval (SetCanceler _ next) = pure next

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
  jarr <- liftWithCanceler' $ Auth.authed $ Api.sample file 0 20
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
           else if (null axises.time) && (length axises.value < 2)
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
  pieBarConfiguration axises current =
    let allAxises = axises.category <> axises.time <> axises.value
        categories =
          setPreviousValueFrom (index current.series 0)
          $ autoSelect $ newSelect allAxises
        measures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends categories axises.value
        firstSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [categories] $ allAxises <-> categories
        secondSeries =
          setPreviousValueFrom (index current.series 2)
          $ newSelect $ ifSelected [categories, firstSeries]
          $ allAxises <-> categories <-> firstSeries
        aggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
    in { series: [categories, firstSeries, secondSeries]
       , dimensions: []
       , measures: [measures]
       , aggregations: [aggregation]
       }

  lineConfiguration :: AxisAccum -> ChartConfiguration -> ChartConfiguration
  lineConfiguration axises current =
    let allAxises = (axises.category <> axises.time <> axises.value)
        dimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ dependsOnArr axises.value
          -- This is redundant, I've put it here to notify
          -- that this behaviour differs from pieBar and can be changed.
          $ allAxises
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
          $ newSelect $ ifSelected [dimensions] $ allAxises <-> dimensions
        secondSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [dimensions, firstSeries]
          $ allAxises <-> dimensions <-> firstSeries
    in { series: [firstSeries, secondSeries]
       , dimensions: [dimensions]
       , measures: [firstMeasures, secondMeasures]
       , aggregations: [aggregationSelect, aggregationSelect]
       }

peek :: forall a. ChildF ChartType Form.QueryP a -> VizDSL Unit
peek (ChildF chartType q) = do
  modify $ _needToUpdate .~ false
  configure
