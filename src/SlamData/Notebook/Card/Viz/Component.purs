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

module SlamData.Notebook.Card.Viz.Component where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (JCursor)
import Data.Array (length, null, cons, index)
import Data.Int as Int
import Data.Lens ((.~), view, preview)
import Data.Map as M
import Data.Path.Pathy (printPath)
import Data.Set as Set

import CSS.Geometry (marginBottom)
import CSS.Size (px)

import Halogen as H
import Halogen.CustomProps as Cp
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Quasar.Types (FilePath)

import SlamData.Effects (Slam)
import SlamData.Form.Select (Select, autoSelect, newSelect, (<->), ifSelected, trySelect', _value)
import SlamData.Notebook.Card.CardType (CardType(Viz))
import SlamData.Notebook.Card.Chart.Aggregation (aggregationSelect)
import SlamData.Notebook.Card.Chart.Axis (analyzeJArray, Axis)
import SlamData.Notebook.Card.Chart.Axis as Ax
import SlamData.Notebook.Card.Chart.ChartConfiguration (ChartConfiguration, depends, dependsOnArr)
import SlamData.Notebook.Card.Chart.ChartOptions (buildOptions)
import SlamData.Notebook.Card.Chart.ChartType (ChartType(..), isPie)
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery(..), CardEvalT, runCardEvalT, liftWithCancelerP')
import SlamData.Notebook.Card.Component (CardStateP, CardQueryP, makeCardComponent, makeQueryPrism', _VizState, _VizQuery)
import SlamData.Notebook.Card.Port as P
import SlamData.Notebook.Card.Viz.Component.Query (QueryC, Query(..))
import SlamData.Notebook.Card.Viz.Component.State (State, _needToUpdate, _availableChartTypes, _sample, fromModel, _records, _loading, _axisLabelFontSize, _axisLabelAngle, _chartType, _width, _height, initialState)
import SlamData.Notebook.Card.Viz.Form.Component (formComponent)
import SlamData.Notebook.Card.Viz.Form.Component as Form
import SlamData.Notebook.Card.Viz.Model as Model
import SlamData.Quasar.Query as Api
import SlamData.Render.Common (row)
import SlamData.Render.CSS as Rc

type VizHTML = H.ParentHTML Form.StateP QueryC Form.QueryP Slam ChartType
type VizDSL = H.ParentDSL State Form.StateP QueryC Form.QueryP Slam ChartType

vizComponent :: H.Component CardStateP CardQueryP Slam
vizComponent = makeCardComponent
  { cardType: Viz
  , component: H.parentComponent { render, eval, peek: Just peek }
  , initialState: H.parentState initialState
  , _State: _VizState
  , _Query: makeQueryPrism' _VizQuery
  }

render :: State -> VizHTML
render state =
  HH.div
    [ HP.classes [ Rc.cardInput ] ]
    [ renderLoading $ not state.loading
    , renderEmpty $ state.loading || (not $ Set.isEmpty state.availableChartTypes)
    , renderForm state
    ]

renderLoading :: Boolean -> VizHTML
renderLoading hidden =
  HH.div
    [ HP.classes
        $ [ B.alert, B.alertInfo, Rc.loadingMessage ]
        <> (guard hidden $> B.hide)
    ]
    [ HH.text "Loading"
    , HH.img [ HP.src "/img/blue-spin.gif" ]
    ]

renderEmpty :: Boolean -> VizHTML
renderEmpty hidden =
  HH.div
    [ HP.classes
        $ [ B.alert, B.alertDanger ]
        <> (guard hidden $> B.hide)
    , CSS.style $ marginBottom $ px 12.0
    ]
    [ HH.text "There is no available chart for this dataset" ]

renderForm :: State -> VizHTML
renderForm state =
  HH.div
    [ HP.classes
        $ [ Rc.vizCardEditor ]
        <> (guard hidden $> B.hide)
    ]
    [ renderChartTypeSelector state
    , renderChartConfiguration state
    ]
  where
  hidden :: Boolean
  hidden = Set.isEmpty state.availableChartTypes || state.loading


renderChartTypeSelector :: State -> VizHTML
renderChartTypeSelector state =
  HH.div
    [ HP.classes [ Rc.vizChartTypeSelector ] ]
    $ foldl (foldFn state.chartType) empty state.availableChartTypes
  where
  foldFn :: ChartType -> Array VizHTML -> ChartType -> Array VizHTML
  foldFn selected accum current =
    flip cons accum $
      HH.img
        [ HP.src $ src current
        , HP.classes
            $ [ cls state.chartType ]
            <> (guard (selected == current) $> B.active)
        , HE.onClick (HE.input_ (right <<< SetChartType current))
        ]

  src :: ChartType -> String
  src Pie = "img/pie.svg"
  src Line = "img/line.svg"
  src Bar = "img/bar.svg"

  cls :: ChartType -> HH.ClassName
  cls Pie = Rc.pieChartIcon
  cls Line = Rc.lineChartIcon
  cls Bar = Rc.barChartIcon


renderChartConfiguration :: State -> VizHTML
renderChartConfiguration state =
  HH.div
    [ HP.classes [ Rc.vizChartConfiguration ] ]
    [ renderTab Pie
    , renderTab Line
    , renderTab Bar
    , renderDimensions state
    ]
  where
  renderTab :: ChartType -> VizHTML
  renderTab ty =
    showIf (state.chartType == ty)
    [ HH.slot ty \_ ->
        { component: formComponent
        , initialState: H.parentState Form.initialState
        }
    ]

  showIf :: Boolean -> Array VizHTML -> VizHTML
  showIf ok content = HH.div [ HP.classes $ (guard (not ok) $> B.hide) ] content


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
    :: HH.ClassName
    -> String
    -> (State -> String)
    -> (Int -> Unit -> Query Unit)
    -> Boolean -> VizHTML
  chartInput cls labelText valueFromState queryCtor isHidden =
    HH.form
      [ HP.classes
          $ [ B.colXs3, cls ]
          <> (guard isHidden $> B.hide)
      , Cp.nonSubmit
      ]
      [ label labelText
      , HH.input
          [ HP.classes [ B.formControl ]
          , HP.value $ valueFromState state
          , ARIA.label labelText
          , Cp.mbValueInput
              $ pure <<< map (right <<< flip queryCtor unit) <<< stringToInt
          ]
      ]

  label :: String -> VizHTML
  label str = HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text str ]

  showIfNeqZero :: forall a. (Eq a, Show a, Semiring a) => a -> String
  showIfNeqZero a = if zero == a then "" else show a

  stringToInt :: String -> Maybe Int
  stringToInt s = if s == "" then Just 0 else Int.fromString s

-- Note: need to put running to state
eval :: Natural QueryC VizDSL
eval = coproduct cardEval vizEval

vizEval :: Natural Query VizDSL
vizEval q = do
  H.modify $ _needToUpdate .~ false
  case q of
    SetHeight h next ->
      H.modify (_height .~ h) *> configure $> next
    SetWidth w next ->
      H.modify (_width .~ w) *> configure $> next
    SetChartType ct next ->
      H.modify (_chartType .~ ct) *> configure $> next
    SetAvailableChartTypes ts next ->
      H.modify (_availableChartTypes .~ ts) *> configure $> next
    RotateAxisLabel angle next ->
      H.modify (_axisLabelAngle .~ angle) *> configure $> next
    SetAxisFontSize size next ->
      H.modify (_axisLabelFontSize .~ size) *> configure $> next

cardEval :: Natural CardEvalQuery VizDSL
cardEval (EvalCard info continue) =
  case info.inputPort of
    Just P.Blocked -> do
      H.modify
        $ (_needToUpdate .~ true)
        <<< (_sample .~ mempty)
        <<< (_records .~ mempty)
        <<< (_availableChartTypes .~ mempty)
      pure $ continue { output: Nothing, messages: [] }
    _ -> do
      needToUpdate <- H.gets _.needToUpdate
      map continue $ runCardEvalT do
        when needToUpdate $ withLoading do
          r <- maybe (throwError "Incorrect port in visual builder card") pure
               $ info.inputPort >>= preview P._Resource
          lift $ updateForms r
          records <-
            Api.all r
              # liftWithCancelerP'
              # lift
              >>= either
                  (const $ throwError $ "Can't get resource: " <> printPath r)
                  pure
          when (length records > 10000)
            $ throwError
            $  "Maximum record count available for visualization -- 10000, "
            <> "please consider using 'limit' or 'group by' in your H.request"
          lift $ H.modify $ _records .~ records
        lift $ H.modify $ _needToUpdate .~ true
        responsePort
  where
  withLoading action = do
    lift $ H.modify $ _loading .~ true
    a <- action
    lift $ H.modify $ _loading .~ false
    pure a
cardEval (SetupCard _ next) = pure next
cardEval (NotifyRunCard next) = pure next
cardEval (NotifyStopCard next) = pure next
cardEval (Save k) = do
  st <- H.get
  config <- H.query st.chartType $ left $ H.request Form.GetConfiguration
  pure $ k $ Model.encode
    { width: st.width
    , height: st.height
    , chartType: st.chartType
    , chartConfig: fromMaybe Form.initialState config
    , axisLabelFontSize: st.axisLabelFontSize
    , axisLabelAngle: st.axisLabelAngle
    }
cardEval (Load json next) =
  case Model.decode json of
    Left err -> pure next
    Right model -> do
      let st = fromModel model
      H.set st
      H.query st.chartType $ left $ H.action $ Form.SetConfiguration model.chartConfig
      pure next
cardEval (SetCanceler _ next) = pure next

responsePort :: CardEvalT VizDSL P.Port
responsePort = do
  state <- lift H.get
  mbConf <- lift $ H.query state.chartType $ left $ H.request Form.GetConfiguration
  conf <- maybe (throwError "Form state has not been set in responsePort")
          pure mbConf
  pure
    $ P.ChartOptions
    { options: buildOptions state conf
    , width: state.width
    , height: state.height
    }

updateForms :: FilePath -> VizDSL Unit
updateForms file = do
  jarr <-
    Api.sample file 0 20
      # liftWithCancelerP'
      >>= either (const $ pure []) pure
  if null jarr
    then
    H.modify $ _availableChartTypes .~ Set.empty
    else do
    H.modify (_sample .~ analyzeJArray jarr)
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
  H.modify (_availableChartTypes .~ available axises)
  where
  getOrInitial :: ChartType -> VizDSL ChartConfiguration
  getOrInitial ty =
    map (fromMaybe Form.initialState)
    $ H.query ty $ left (H.request Form.GetConfiguration)

  setConfFor :: ChartType -> ChartConfiguration -> VizDSL Unit
  setConfFor ty conf =
    void $ H.query ty $ left $ H.action $ Form.SetConfiguration conf

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
    sample <- H.gets _.sample
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

peek :: forall a. H.ChildF ChartType Form.QueryP a -> VizDSL Unit
peek _ = do
  H.modify $ _needToUpdate .~ false
  configure
