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

module SlamData.Workspace.Card.Viz.Component where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array (length, null, cons, index)
import Data.Int as Int
import Data.Lens as Lens
import Data.Lens ((.~), (^?))
import Data.Map as M
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

import SlamData.Effects (Slam)
import SlamData.Form.Select (Select, autoSelect, newSelect, (⊝), ifSelected, trySelect', _value)
import SlamData.Workspace.Card.CardType (CardType(Viz))
import SlamData.Workspace.Card.Chart.Aggregation (aggregationSelect)
import SlamData.Workspace.Card.Chart.Axis (analyzeJArray, Axis)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration, depends, dependsOnArr)
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..), isPie)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery(..))
import SlamData.Workspace.Card.Component (CardStateP, CardQueryP, makeCardComponent, makeQueryPrism', _VizState, _VizQuery)
import SlamData.Workspace.Card.Port as P
import SlamData.Workspace.Card.Viz.Component.Query (QueryC, Query(..))
import SlamData.Workspace.Card.Viz.Component.State as VCS
import SlamData.Workspace.Card.Viz.Form.Component (formComponent)
import SlamData.Workspace.Card.Viz.Form.Component as Form
import SlamData.Workspace.Card.Viz.Model as Model
import SlamData.Render.Common (row)
import SlamData.Render.CSS as Rc

type VizHTML = H.ParentHTML Form.StateP QueryC Form.QueryP Slam ChartType
type VizDSL = H.ParentDSL VCS.State Form.StateP QueryC Form.QueryP Slam ChartType

-- | How does this module work?
-- | + Take a TaggedResource case of Port
-- | + Check that resource exists and if so analyze sample to extract Map from
-- |   JCursors to Axes otherwise leave it Map.empty and set available chart types
-- |   to Set.empty. Following steps don't do anything useful if sample is empty.
-- |   Localized in first part of EvalCard handler and updateForm function.
-- | + Load all resource and if it's not too big produce output port
-- |   Second part of EvalCard handler
-- |
-- | + Output port is response of currently active subcomponent which is `Form`
-- | + Form's state (ChartConfiguration) is record with `dimensions`,
-- |   `aggregations`, `series` and `measures` fields.
-- |   These fields are arrays of `Select JCursor`  where `Select α`
-- |   is model of html combobox with maybe selected α and list of α choices.
-- | + Form can render any kind of `ChartConfiguration` and has weird logic
-- |   for doing this :)
-- |
-- | + Peeking form signals and running CardEval call `configure` func
-- |   (second through `updateForms`)
-- | + `configure` takes all subcomponent configuration and filters them
-- |   e.g. it removes already selected in first measure combobox value from
-- |   available to select choices of second measure combobox -->
-- |   nonsense output is forbidden (one shouldn't be able to make chart
-- |   from `foo` to `foo` groupped by `foo`)
-- | + After filtering (That's important!) previoiusly selected values must
-- |   be set with func `setPreviousValueFrom`.
-- | + Then we have updated config and set it back as subcomponent state.
-- |
-- | About `needToUpdate`
-- | This flag is set when we need to update `records` and `sample` fields.
-- | Basically it's true after parent in deck has changed its output.
-- | And it's false when we just re`configure`d subcomponents.
-- |
-- |   >>> TODO: update this note, or fix the code to restore the old needToUpdate
-- |       logic, if needed. -js
-- |
-- | cryogenian 04/29/2016


vizComponent ∷ H.Component CardStateP CardQueryP Slam
vizComponent = makeCardComponent
  { cardType: Viz
  , component: H.parentComponent { render, eval, peek: Just peek }
  , initialState: H.parentState VCS.initialState
  , _State: _VizState
  , _Query: makeQueryPrism' _VizQuery
  }

render ∷ VCS.State → VizHTML
render state =
  HH.div
    [ HP.classes [ Rc.cardInput ] ]
    [ renderLoading $ not state.loading
    , renderEmpty $ state.loading || (not $ Set.isEmpty state.availableChartTypes)
    , renderForm state
    ]

renderLoading ∷ Boolean → VizHTML
renderLoading hidden =
  HH.div
    [ HP.classes
        $ [ B.alert, B.alertInfo, Rc.loadingMessage ]
        ⊕ (guard hidden $> B.hide)
    ]
    [ HH.text "Loading"
    , HH.img [ HP.src "/img/blue-spin.gif" ]
    ]

renderEmpty ∷ Boolean → VizHTML
renderEmpty hidden =
  HH.div
    [ HP.classes
        $ [ B.alert, B.alertDanger ]
        ⊕ (guard hidden $> B.hide)
    , CSS.style $ marginBottom $ px 12.0
    ]
    [ HH.text "There is no available chart for this dataset" ]

renderForm ∷ VCS.State → VizHTML
renderForm state =
  HH.div
    [ HP.classes
        $ [ Rc.vizCardEditor ]
        ⊕ (guard hidden $> B.hide)
    ]
    [ renderChartTypeSelector state
    , renderChartConfiguration state
    ]
  where
  hidden ∷ Boolean
  hidden = Set.isEmpty state.availableChartTypes || state.loading


renderChartTypeSelector ∷ VCS.State → VizHTML
renderChartTypeSelector state =
  HH.div
    [ HP.classes [ Rc.vizChartTypeSelector ] ]
    $ foldl (foldFn state.chartType) empty state.availableChartTypes
  where
  foldFn ∷ ChartType → Array VizHTML → ChartType → Array VizHTML
  foldFn selected accum current =
    flip cons accum $
      HH.img
        [ HP.src $ src current
        , HP.classes
            $ [ cls state.chartType ]
            ⊕ (guard (selected ≡ current) $> B.active)
        , HE.onClick (HE.input_ (right ∘ SetChartType current))
        ]

  src ∷ ChartType → String
  src Pie = "img/pie.svg"
  src Line = "img/line.svg"
  src Bar = "img/bar.svg"

  cls ∷ ChartType → HH.ClassName
  cls Pie = Rc.pieChartIcon
  cls Line = Rc.lineChartIcon
  cls Bar = Rc.barChartIcon


renderChartConfiguration ∷ VCS.State → VizHTML
renderChartConfiguration state =
  HH.div
    [ HP.classes [ Rc.vizChartConfiguration ] ]
    [ renderTab Pie
    , renderTab Line
    , renderTab Bar
    , renderDimensions state
    ]
  where
  renderTab ∷ ChartType → VizHTML
  renderTab ty =
    showIf (state.chartType ≡ ty)
    [ HH.slot ty \_ →
        { component: formComponent
        , initialState: H.parentState Form.initialState
        }
    ]

  showIf ∷ Boolean → Array VizHTML → VizHTML
  showIf ok content = HH.div [ HP.classes $ (guard (not ok) $> B.hide) ] content


renderDimensions ∷ VCS.State → VizHTML
renderDimensions state =
  row
  [ chartInput Rc.chartSizeParam "Height"
      (_.height ⋙ showIfNeqZero) SetHeight false
  , chartInput Rc.chartSizeParam "Width"
      (_.width ⋙ showIfNeqZero) SetWidth false
  , chartInput Rc.axisLabelParam "Axis label angle"
      (_.axisLabelAngle ⋙ show) RotateAxisLabel (isPie state.chartType)
  , chartInput Rc.axisLabelParam "Axis font size"
      (_.axisLabelFontSize ⋙ show) SetAxisFontSize (isPie state.chartType)
  ]
  where
  chartInput
    ∷ HH.ClassName
    → String
    → (VCS.State → String)
    → (Int → Unit → Query Unit)
    → Boolean → VizHTML
  chartInput cls labelText valueFromState queryCtor isHidden =
    HH.form
      [ HP.classes
          $ [ B.colXs3, cls ]
          ⊕ (guard isHidden $> B.hide)
      , Cp.nonSubmit
      ]
      [ label labelText
      , HH.input
          [ HP.classes [ B.formControl ]
          , HP.value $ valueFromState state
          , ARIA.label labelText
          , HE.onValueInput
              $ pure ∘ map (right ∘ flip queryCtor unit) ∘ stringToInt
          ]
      ]

  label ∷ String → VizHTML
  label str = HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text str ]

  showIfNeqZero ∷ ∀ a. (Eq a, Show a, Semiring a) ⇒ a → String
  showIfNeqZero a = if zero ≡ a then "" else show a

  stringToInt ∷ String → Maybe Int
  stringToInt s = if s ≡ "" then Just 0 else Int.fromString s

-- Note: need to put running to state
eval ∷ QueryC ~> VizDSL
eval = coproduct cardEval vizEval

vizEval ∷ Query ~> VizDSL
vizEval q = do
  H.modify $ VCS._needToUpdate .~ false
  case q of
    SetHeight h next →
      H.modify (VCS._height .~ h) *> configure $> next
    SetWidth w next →
      H.modify (VCS._width .~ w) *> configure $> next
    SetChartType ct next →
      H.modify (VCS._chartType .~ ct) *> configure $> next
    SetAvailableChartTypes ts next →
      H.modify (VCS._availableChartTypes .~ ts) *> configure $> next
    RotateAxisLabel angle next →
      H.modify (VCS._axisLabelAngle .~ angle) *> configure $> next
    SetAxisFontSize size next →
      H.modify (VCS._axisLabelFontSize .~ size) *> configure $> next

cardEval ∷ CardEvalQuery ~> VizDSL
cardEval (EvalCard info output next) = do
  traceAnyA {output}
  -- TODO: not sure how to restore the "needToUpdate" logic that used to be here. -js
  for (output ^? Lens._Just ∘ P._ChartOptions) \opts → do
    if null opts.recordsSample
      then H.modify (VCS._availableChartTypes .~ Set.empty)
      else H.modify (VCS._sample .~ analyzeJArray opts.recordsSample) *> configure
    H.modify $ VCS._records .~ opts.records

  -- TODO: find a way to "bracket" the loading state like we did before. It is not
  -- clear how to do this at the moment, since the actual activity of loading the
  -- data takes place exterinallyin the model eval machinery.
  H.modify $ VCS._loading .~ false
  pure next
cardEval (SetupCard _ next) = pure next
cardEval (NotifyRunCard next) = pure next
cardEval (NotifyStopCard next) = pure next
cardEval (Save k) = do
  st ← H.get
  config ← H.query st.chartType $ left $ H.request Form.GetConfiguration
  pure $ k $ Model.encode
    { width: st.width
    , height: st.height
    , chartType: st.chartType
    , chartConfig: fromMaybe Form.initialState config
    , axisLabelFontSize: st.axisLabelFontSize
    , axisLabelAngle: st.axisLabelAngle
    , records: st.records
    }
cardEval (Load json next) =
  next <$ for_ (Model.decode json) \model → do
    let st = VCS.fromModel model
    H.set st
    H.query st.chartType
      $ left
      $ H.action $ Form.SetConfiguration model.chartConfig
cardEval (SetCanceler _ next) = pure next
cardEval (SetDimensions _ next) = pure next

type AxisAccum =
  { category ∷ Array JCursor
  , value ∷ Array JCursor
  , time ∷ Array JCursor
  }

configure ∷ VizDSL Unit
configure = void do
  axises ← getAxises
  pieConf ← getOrInitial Pie
  setConfFor Pie $ pieBarConfiguration axises pieConf
  lineConf ← getOrInitial Line
  setConfFor Line $ lineConfiguration axises lineConf
  barConf ← getOrInitial Bar
  setConfFor Bar $ pieBarConfiguration axises barConf
  H.modify (VCS._availableChartTypes .~ available axises)
  where
  getOrInitial ∷ ChartType → VizDSL ChartConfiguration
  getOrInitial ty =
    map (fromMaybe Form.initialState)
      $ H.query ty
      $ left (H.request Form.GetConfiguration)

  setConfFor ∷ ChartType → ChartConfiguration → VizDSL Unit
  setConfFor ty conf =
    void $ H.query ty $ left $ H.action $ Form.SetConfiguration conf

  available ∷ AxisAccum → Set.Set ChartType
  available axises =
    foldMap Set.singleton
    $ if null axises.value
      then []
      else if not $ null axises.category
           then [Pie, Bar, Line]
           else if (null axises.time) && (length axises.value < 2)
                then []
                else [Line]

  getAxises ∷ VizDSL AxisAccum
  getAxises = do
    sample ← H.gets _.sample
    pure $ foldl axisFolder {category: [], value: [], time: [] } $ M.toList sample

  axisFolder ∷ AxisAccum → Tuple JCursor Axis → AxisAccum
  axisFolder accum (Tuple cursor axis)
    | Ax.isCatAxis axis = accum { category = cons cursor accum.category }
    | Ax.isValAxis axis = accum { value = cons cursor accum.value }
    | Ax.isTimeAxis axis = accum {time = cons cursor accum.time }
    | otherwise = accum


  setPreviousValueFrom
    ∷ ∀ a. (Eq a) ⇒ Maybe (Select a) → Select a → Select a
  setPreviousValueFrom mbSel target  =
    (maybe id trySelect' $ mbSel >>= Lens.view _value) $ target

  pieBarConfiguration ∷ AxisAccum → ChartConfiguration → ChartConfiguration
  pieBarConfiguration axises current =
    let allAxises = axises.category ⊕ axises.time ⊕ axises.value
        categories =
          setPreviousValueFrom (index current.series 0)
          $ autoSelect $ newSelect allAxises
        measures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends categories axises.value
        firstSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [categories] $ allAxises ⊝ categories
        secondSeries =
          setPreviousValueFrom (index current.series 2)
          $ newSelect $ ifSelected [categories, firstSeries]
          $ allAxises ⊝ categories ⊝ firstSeries
        aggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
    in { series: [categories, firstSeries, secondSeries]
       , dimensions: []
       , measures: [measures]
       , aggregations: [aggregation]
       }

  lineConfiguration ∷ AxisAccum → ChartConfiguration → ChartConfiguration
  lineConfiguration axises current =
    let allAxises = (axises.category ⊕ axises.time ⊕ axises.value)
        dimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ dependsOnArr axises.value
          -- This is redundant, I've put it here to notify
          -- that this behaviour differs from pieBar and can be changed.
          $ allAxises
        firstMeasures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends dimensions
          $ axises.value ⊝ dimensions
        secondMeasures =
          setPreviousValueFrom (index current.measures 1)
          $ newSelect $ ifSelected [firstMeasures]
          $ depends dimensions
          $ axises.value ⊝ firstMeasures ⊝ dimensions
        firstSeries =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [dimensions] $ allAxises ⊝ dimensions
        secondSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [dimensions, firstSeries]
          $ allAxises ⊝ dimensions ⊝ firstSeries
        firstAggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
        secondAggregation =
          setPreviousValueFrom (index current.aggregations 1) aggregationSelect
    in { series: [firstSeries, secondSeries]
       , dimensions: [dimensions]
       , measures: [firstMeasures, secondMeasures]
       , aggregations: [firstAggregation, secondAggregation]
       }

peek ∷ ∀ a. H.ChildF ChartType Form.QueryP a → VizDSL Unit
peek _ = do
  H.modify $ VCS._needToUpdate .~ false
  configure
