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

module SlamData.Workspace.Card.ChartOptions.Component (chartOptionsComponent) where

import SlamData.Prelude

import Data.Array (cons, index)
import Data.Foldable as F
import Data.Int as Int
import Data.Lens as Lens
import Data.Lens ((.~), (^?))
import Data.List as L
import Data.Set as Set
import Global (readFloat, isNaN)

import CSS.Geometry (marginBottom)
import CSS.Size (px)

import Halogen as H
import Halogen.CustomProps as Cp
import Halogen.HTML.CSS.Indexed as HCSS
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Form.Select (autoSelect, newSelect, (⊝), ifSelected, isSelected, setPreviousValueFrom)
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.CardType (CardType(ChartOptions))
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Chart.Aggregation (aggregationSelect, aggregationSelectWithNone)
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colorSchemes, printColorScheme)
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration, depends, dependsOnArr)
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..), isPie, isArea, isScatter, isRadar, isFunnel, isGraph, isHeatmap)
import SlamData.Workspace.Card.Chart.Config as CH
import SlamData.Workspace.Card.ChartOptions.Component.CSS as CSS
import SlamData.Workspace.Card.ChartOptions.Component.Query (QueryC, Query(..))
import SlamData.Workspace.Card.ChartOptions.Component.State as VCS
import SlamData.Workspace.Card.ChartOptions.Form.Component (formComponent)
import SlamData.Workspace.Card.ChartOptions.Form.Component as Form
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as P
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.ChartOptions.Graph.Component as Graph
import SlamData.Workspace.Card.ChartOptions.Component.Install (ChildState, ChildQuery, ChildSlot, cpForm, cpGraph)

type DSL = H.ParentDSL VCS.State ChildState QueryC ChildQuery Slam ChildSlot
type HTML = H.ParentHTML ChildState QueryC ChildQuery Slam ChildSlot


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
-- |   Is model of html combobox with maybe selected α and list of α choices.
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


chartOptionsComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
chartOptionsComponent = CC.makeCardComponent
  { cardType: ChartOptions
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState VCS.initialState
  , _State: CC._ChartOptionsState
  , _Query: CC.makeQueryPrism' CC._ChartOptionsQuery
  }

render ∷ VCS.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.lightCardGlyph CT.ChartOptions) left state.levelOfDetails
    ]

renderHighLOD ∷ VCS.State → HTML
renderHighLOD state =
    HH.div
      [ HP.classes
          $ [ CSS.cardInput, HH.className "card-input-maximum-lod" ]
          ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
      ]
      [ renderEmpty $ not Set.isEmpty state.availableChartTypes
      , renderForm state
      ]

renderEmpty ∷ Boolean → HTML
renderEmpty hidden =
  HH.div
    [ HP.classes
        $ [ B.alert, B.alertDanger ]
        ⊕ (guard hidden $> B.hide)
    , HCSS.style $ marginBottom $ px 12.0
    ]
    [ HH.text "There is no available chart for this dataset" ]

renderForm ∷ VCS.State → HTML
renderForm state =
  HH.div
    [ HP.classes
        $ [ CSS.vizCardEditor ]
        ⊕ (guard hidden $> B.hide)
    ]
    [ renderChartTypeSelector state
    , renderChartConfiguration state
    ]
  where
  hidden ∷ Boolean
  hidden = Set.isEmpty state.availableChartTypes

renderChartTypeSelector ∷ VCS.State → HTML
renderChartTypeSelector state =
  HH.div
    [ HP.classes [ CSS.vizChartTypeSelector ] ]
    $ foldl (foldFn state.chartType) empty state.availableChartTypes
  where
  foldFn ∷ ChartType → Array HTML → ChartType → Array HTML
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
  src Area = "img/area.svg"
  src Scatter = "img/scatter.svg"
  src Radar = "img/radar.svg"
  src Funnel = "img/funnel.svg"
  src Graph = "img/graph.svg"
  src Heatmap = "img/heatmap.svg"

  cls ∷ ChartType → HH.ClassName
  cls Pie = CSS.pieChartIcon
  cls Line = CSS.lineChartIcon
  cls Bar = CSS.barChartIcon
  cls Area = CSS.areaChartIcon
  cls Scatter = CSS.scatterChartIcon
  cls Radar = CSS.radarChartIcon
  cls Funnel = CSS.funnelChartIcon
  cls Graph = CSS.pieChartIcon
  cls Heatmap = CSS.heatmapChartIcon

renderChartConfiguration ∷ VCS.State → HTML
renderChartConfiguration state =
  HH.div
    [ HP.classes [ CSS.vizChartConfiguration ] ]
    [ renderTab Pie
    , renderTab Line
    , renderTab Bar
    , renderTab Area
    , renderTab Scatter
    , renderTab Radar
    , renderTab Funnel
    , renderTab Graph
    , renderTab Heatmap
    , renderDimensions state
    ]
  where
  renderTab ∷ ChartType → HTML
  renderTab Graph =
    showIf (state.chartType ≡ Graph)
    [ HH.slot' cpGraph unit \_ →
         { component: Graph.comp
         , initialState: H.parentState Graph.initialState
         }
    ]
  renderTab ty =
    showIf (state.chartType ≡ ty)
    [ HH.slot' cpForm ty \_ →
        { component: formComponent
        , initialState: H.parentState $ Form.getInitialState ty
        }
    ]

  showIf ∷ Boolean → Array HTML → HTML
  showIf ok content = HH.div [ HP.classes $ (guard (not ok) $> B.hide) ] content


renderDimensions ∷ VCS.State → HTML
renderDimensions state =
  row
  [ intChartInput CSS.axisLabelParam "Axis label angle"
      (_.axisLabelAngle ⋙ show) RotateAxisLabel
      (F.any (_ $ state.chartType) [ isPie, isScatter, isRadar, isFunnel, isGraph, isHeatmap ] )
  , intChartInput CSS.axisLabelParam "Axis font size"
      (_.axisLabelFontSize ⋙ show) SetAxisFontSize
      (F.any (_ $ state.chartType) [ isPie, isScatter, isRadar, isFunnel, isGraph, isHeatmap ] )
  , boolChartInput CSS.chartDetailParam "If stack"
      (_.areaStacked) ToggleSetStacked (not $ isArea state.chartType)
  , boolChartInput CSS.chartDetailParam "If smooth"
      (_.smooth) ToggleSetSmooth (not $ isArea state.chartType)
  , numChartInput CSS.axisLabelParam "Min size of circle"
      (_.bubbleMinSize) Nothing (Just _.bubbleMaxSize) (Just 0.0) Nothing
        SetBubbleMinSize (not $ isScatter state.chartType)
  , numChartInput CSS.axisLabelParam "Max size of circle"
      (_.bubbleMaxSize) (Just _.bubbleMinSize) Nothing Nothing Nothing
        SetBubbleMaxSize (not $ isScatter state.chartType)
  , optionSelect CSS.funnelChartOrderParam "Order" ["ascending", "descending"]
      (_.funnelOrder) SetFunnelOrder (not $ isFunnel state.chartType)
  , optionSelect CSS.funnelChartAlignParam "Alignment" ["right", "left", "center"]
      (_.funnelAlign) SetFunnelAlign (not $ isFunnel state.chartType)
  , numChartInput CSS.axisLabelParam "Min color rendering value"
      (_.minColorVal) Nothing (Just _.maxColorVal) Nothing Nothing
        SetMinColorVal (not $ isHeatmap state.chartType)
  , numChartInput CSS.axisLabelParam "Max color rendering value"
      (_.maxColorVal) (Just _.minColorVal) Nothing Nothing Nothing
        SetMaxColorVal (not $ isHeatmap state.chartType)
  , optionSelect CSS.axisLabelParam "Color scheme" (map printColorScheme colorSchemes)
      (_.colorScheme) SetColorScheme (not $ isHeatmap state.chartType)
  , boolChartInput CSS.chartDetailParam "If reverse color"
      (_.colorReversed) SetColorReversed (not $ isHeatmap state.chartType)
  ]
  where
  optionSelect
    ∷ HH.ClassName
    → String
    → Array String
    → (VCS.State → String)
    → (String → Unit → Query Unit)
    → Boolean → HTML
  optionSelect cls labelText opts valueFromState queryCtor isHidden =
    HH.form
      [ HP.classes
          $ [ B.colXs6, cls ]
          ⊕ (guard isHidden $> B.hide)
      , Cp.nonSubmit
      ]
      [ label labelText
      , HH.select
        [ HP.classes [ B.formControl ]
        , HE.onValueChange
            $ pure ∘ map (right ∘ flip queryCtor unit) ∘ Just
        ]
        (options opts (valueFromState state))
      ]
    where
    options ∷ Array String → String → Array HTML
    options arrStr currentVal = map mkOption arrStr
      where
      mkOption ∷ String → HTML
      mkOption val =
        HH.option
          [ HP.selected (val == currentVal)
          , HP.value val
          ]
          [ HH.text val ]

  intChartInput
    ∷ HH.ClassName
    → String
    → (VCS.State → String)
    → (Int → Unit → Query Unit)
    → Boolean → HTML
  intChartInput cls labelText valueFromState queryCtor isHidden =
    HH.form
      [ HP.classes
          $ [ B.colXs6, cls ]
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
    where
    stringToInt ∷ String → Maybe Int
    stringToInt s = if s ≡ "" then Just 0 else Int.fromString s

  numChartInput
    ∷ HH.ClassName
    → String
    → (VCS.State → Number)
    → Maybe (VCS.State → Number)
    → Maybe (VCS.State → Number)
    → Maybe Number
    → Maybe Number
    → (Number → Unit → Query Unit)
    → Boolean → HTML
  numChartInput
    cls
    labelText
    getCurrentVal
    getLowerBoudary
    getUpperBoudary
    minVal
    maxVal
    queryCtor
    isHidden = do
    let
      lowerBoudary = case getLowerBoudary × minVal of
        Just func × Just v → if (func state) < v then Just $ func state else Just v
        Nothing × Just v → Just v
        Just func × Nothing → Just $ func state
        _ → Nothing
      uppperBoudary = case getUpperBoudary × maxVal of
        Just func × Just v → if (func state) > v then Just $ func state else Just v
        Nothing × Just v → Just v
        Just func × Nothing → Just $ func state
        _ → Nothing
    HH.form
      [ HP.classes
          $ [ B.colXs6, cls ]
          ⊕ (guard isHidden $> B.hide)
      , Cp.nonSubmit
      ]
      [ label labelText
      , HH.input
          [ HP.classes [ B.formControl ]
          , HP.value $ show $ getCurrentVal state
          , ARIA.label labelText
          , HE.onValueChange
              $ pure ∘ map (right ∘ flip queryCtor unit) ∘
                stringToNum (getCurrentVal state) lowerBoudary uppperBoudary
          ]
      ]
    where
    stringToNum
      ∷ Number
      → Maybe Number
      → Maybe Number
      → String
      → Maybe Number
    stringToNum currentVal lowerBoudary uppperBoudary s = do
      let newVal = readFloat s
      if isNaN $ newVal then Just currentVal
        else case lowerBoudary × uppperBoudary of
          Just l × Just u → if (newVal >= l ∧ newVal <= u) then Just newVal else Just currentVal
          Nothing × Just u → if newVal <= u then Just newVal else Just currentVal
          Just l × Nothing → if newVal >= l then Just newVal else Just currentVal
          _ → Just newVal

  boolChartInput
    ∷ HH.ClassName
    → String
    → (VCS.State → Boolean)
    → (Boolean → Unit → Query Unit)
    → Boolean → HTML
  boolChartInput cls labelText valueFromState queryCtor isHidden =
    HH.form
      [ HP.classes
          $ [ B.colXs6, cls ]
          ⊕ (guard isHidden $> B.hide)
      , Cp.nonSubmit
      ]
      [ label labelText
      , HH.input
          [ HP.inputType HP.InputCheckbox
          , HP.checked $ valueFromState state
          , ARIA.label labelText
          , HE.onChecked
             $ HE.input_ (right ∘ queryCtor (not $ valueFromState state))
          ]
      ]

  label ∷ String → HTML
  label str = HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text str ]

  showIfNeqZero ∷ ∀ a. (Eq a, Show a, Semiring a) ⇒ a → String
  showIfNeqZero a = if zero ≡ a then "" else show a

-- Note: need to put running to state
eval ∷ QueryC ~> DSL
eval = cardEval ⨁ chartEval

chartEval ∷ Query ~> DSL
chartEval q = do
  next ← case q of
    SetChartType ct n → H.modify (VCS._chartType .~ ct) $> n
    RotateAxisLabel angle n → H.modify (VCS._axisLabelAngle .~ angle) $> n
    SetAxisFontSize size n → H.modify (VCS._axisLabelFontSize .~ size) $> n
    ToggleSetStacked stacked n → H.modify (VCS._areaStacked .~ stacked) $> n
    ToggleSetSmooth smooth n → H.modify (VCS._smooth .~ smooth) $> n
    SetBubbleMinSize bubbleMinSize n → H.modify (VCS._bubbleMinSize .~ bubbleMinSize) $> n
    SetBubbleMaxSize bubbleMaxSize n → H.modify (VCS._bubbleMaxSize .~ bubbleMaxSize) $> n
    SetFunnelOrder funnelOrder n → H.modify (VCS._funnelOrder .~ funnelOrder) $> n
    SetFunnelAlign funnelAlign n → H.modify (VCS._funnelAlign .~ funnelAlign) $> n
    SetMinColorVal minColorVal n → H.modify (VCS._minColorVal .~ minColorVal) $> n
    SetMaxColorVal maxColorVal n → H.modify (VCS._maxColorVal .~ maxColorVal) $> n
    SetColorScheme colorScheme n → H.modify (VCS._colorScheme .~ colorScheme) $> n
    SetColorReversed colorReversed n → H.modify (VCS._colorReversed .~ colorReversed) $> n
  configure
  CC.raiseUpdatedP' CC.EvalModelUpdate
  pure next

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.EvalCard info output next → do
    for_ (output ^? Lens._Just ∘ P._Chart) \opts → do
      H.modify
        $ (VCS._availableChartTypes .~ opts.availableChartTypes)
        ∘ (VCS._axes .~ opts.axes)
      case L.fromFoldable opts.availableChartTypes of
        L.Cons ct L.Nil → H.modify (VCS._chartType .~ ct)
        _ → pure unit
      configure
    for_ (output ^? Lens._Just ∘ P._CardError) \_ →
      H.modify
        $ (VCS._availableChartTypes .~ Set.empty)
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    chartConfig ← case st.chartType of
      Graph →
        map join $ H.query' cpGraph unit $ left $ H.request Graph.GetChartConfig
      _ → do
        conf ← H.query' cpForm st.chartType $ left $ H.request Form.GetConfiguration
        let
          rawConfig = fromMaybe Form.initialConfiguration conf
          mbChartCfg = case st.chartType of
            Pie | not $ F.any isSelected rawConfig.series → Nothing
            Pie | not $ F.any isSelected rawConfig.measures → Nothing
            Bar | not $ F.any isSelected rawConfig.series → Nothing
            Bar | not $ F.any isSelected rawConfig.measures → Nothing
            Line | not $ F.any isSelected rawConfig.dimensions → Nothing
            Line | not $ F.any isSelected rawConfig.measures → Nothing
            Area | not $ F.any isSelected rawConfig.dimensions → Nothing
            Area | not $ F.any isSelected rawConfig.measures → Nothing
            Scatter | not $ F.any isSelected rawConfig.measures → Nothing
            Radar | not $ F.any isSelected rawConfig.dimensions → Nothing
            Radar | not $ F.any isSelected rawConfig.measures → Nothing
            Funnel | not $ F.any isSelected rawConfig.dimensions → Nothing
            Funnel | not $ F.any isSelected rawConfig.measures → Nothing
            Heatmap | not $ F.any isSelected rawConfig.dimensions → Nothing
            Heatmap | not $ F.any isSelected rawConfig.measures → Nothing
            _ → Just rawConfig
        pure
          $ mbChartCfg
          <#> { chartConfig: _
              , options:
                 { chartType: st.chartType
                 , axisLabelFontSize: st.axisLabelFontSize
                 , axisLabelAngle: st.axisLabelAngle
                 , areaStacked: st.areaStacked
                 , smooth: st.smooth
                 , bubbleMinSize: st.bubbleMinSize
                 , bubbleMaxSize: st.bubbleMaxSize
                 , funnelOrder: st.funnelOrder
                 , funnelAlign: st.funnelAlign
                 , minColorVal: st.minColorVal
                 , maxColorVal: st.maxColorVal
                 , colorScheme: st.colorScheme
                 , colorReversed: st.colorReversed
                 }
              }
          <#> CH.Legacy

    pure ∘ k $ Card.ChartOptions chartConfig

  CC.Load card next → do
    -- TODO: Load graph
    case card of
      Card.ChartOptions model → do
        let st = VCS.fromModel model
        H.set st
        for_ (model >>= Lens.preview CH._Legacy) \{chartConfig} →
          H.query' cpForm st.chartType
            $ left
            $ H.action $ Form.SetConfiguration chartConfig
        for_ (model >>= Lens.preview CH._Graph) \graphConfig →
          H.query' cpGraph unit $ left $ H.action $ Graph.Load graphConfig
      _ → pure unit
    pure next
  CC.SetDimensions dims next → do
    H.modify
      $ VCS._levelOfDetails
      .~ if dims.width < 576.0 ∨ dims.height < 416.0
           then Low
           else High
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

configure ∷ DSL Unit
configure = void do
  axes ← H.gets _.axes

  H.query' cpGraph unit $ left $ H.action $ Graph.UpdateAxes axes

  pieConf ← getOrInitial Pie
  setConfigFor Pie $ pieBarConfiguration axes pieConf

  lineConf ← getOrInitial Line
  setConfigFor Line $ lineConfiguration axes lineConf

  barConf ← getOrInitial Bar
  setConfigFor Bar $ pieBarConfiguration axes barConf

  areaConf ← getOrInitial Area
  setConfigFor Area $ areaConfiguration axes areaConf

  scatterConf ← getOrInitial Scatter
  setConfigFor Scatter $ scatterConfiguration axes scatterConf

  radarConf ← getOrInitial Radar
  setConfigFor Radar $ radarConfiguration axes radarConf

  funnelConf ← getOrInitial Funnel
  setConfigFor Funnel $ funnelConfiguration axes funnelConf
  heatmapConf ← getOrInitial Heatmap
  setConfigFor Heatmap $ heatmapConfiguration axes heatmapConf
  where
  getOrInitial ∷ ChartType → DSL ChartConfiguration
  getOrInitial ty =
    map (fromMaybe Form.initialConfiguration)
      $ H.query' cpForm ty
      $ left (H.request Form.GetConfiguration)

  setConfigFor ∷ ChartType → ChartConfiguration → DSL Unit
  setConfigFor ty conf =
    void $ H.query' cpForm ty $ left $ H.action $ Form.SetConfiguration conf


  pieBarConfiguration ∷ Axes → ChartConfiguration → ChartConfiguration
  pieBarConfiguration axes current =
    let allAxes = axes.category ⊕ axes.time ⊕ axes.value
        categories =
          setPreviousValueFrom (index current.series 0)
          $ autoSelect $ newSelect allAxes
        measures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends categories axes.value
        firstSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [categories] $ allAxes ⊝ categories
        secondSeries =
          setPreviousValueFrom (index current.series 2)
          $ newSelect $ ifSelected [categories, firstSeries]
          $ allAxes ⊝ categories ⊝ firstSeries
        aggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
    in { series: [categories, firstSeries, secondSeries]
       , dimensions: []
       , measures: [measures]
       , aggregations: [aggregation]
       }

  lineConfiguration ∷ Axes → ChartConfiguration → ChartConfiguration
  lineConfiguration axes current =
    let allAxes = (axes.category ⊕ axes.time ⊕ axes.value)
        dimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ dependsOnArr axes.value
          -- This is redundant, I've put it here to notify
          -- that this behaviour differs from pieBar and can be changed.
          $ allAxes
        firstMeasures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends dimensions
          $ axes.value ⊝ dimensions
        secondMeasures =
          setPreviousValueFrom (index current.measures 1)
          $ newSelect $ ifSelected [firstMeasures]
          $ depends dimensions
          $ axes.value ⊝ firstMeasures ⊝ dimensions
        firstSeries =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [dimensions] $ allAxes ⊝ dimensions
        secondSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [dimensions, firstSeries]
          $ allAxes ⊝ dimensions ⊝ firstSeries
        firstAggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
        secondAggregation =
          setPreviousValueFrom (index current.aggregations 1) aggregationSelect
    in { series: [firstSeries, secondSeries]
       , dimensions: [dimensions]
       , measures: [firstMeasures, secondMeasures]
       , aggregations: [firstAggregation, secondAggregation]
       }

  areaConfiguration ∷ Axes → ChartConfiguration → ChartConfiguration
  areaConfiguration axes current =
    let allAxes = (axes.category ⊕ axes.time ⊕ axes.value)
        dimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ dependsOnArr axes.value
          -- This is redundant, I've put it here to notify
          -- that this behaviour differs from pieBar and can be changed.
          $ allAxes
        firstMeasures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends dimensions
          $ axes.value ⊝ dimensions
        secondMeasures =
          setPreviousValueFrom (index current.measures 1)
          $ newSelect $ ifSelected [firstMeasures]
          $ depends dimensions
          $ axes.value ⊝ firstMeasures ⊝ dimensions
        firstSeries =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [dimensions] $ allAxes ⊝ dimensions
        secondSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [dimensions, firstSeries]
          $ allAxes ⊝ dimensions ⊝ firstSeries
        firstAggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
        secondAggregation =
          setPreviousValueFrom (index current.aggregations 1) aggregationSelect
    in { series: [firstSeries, secondSeries]
       , dimensions: [dimensions]
       , measures: [firstMeasures, secondMeasures]
       , aggregations: [firstAggregation, secondAggregation]
       }

  scatterConfiguration ∷ Axes → ChartConfiguration → ChartConfiguration
  scatterConfiguration axes current =
    let allAxises = (axes.category ⊕ axes.time ⊕ axes.value)
        firstMeasures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ axes.value
        secondMeasures =
          setPreviousValueFrom (index current.measures 1)
          $ autoSelect $ newSelect $ depends firstMeasures
          $ axes.value ⊝ firstMeasures
        thirdMeasures =
          setPreviousValueFrom (index current.measures 2)
          $ autoSelect $ newSelect $ axes.value
        firstSeries =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [secondMeasures]
          $ allAxises
        secondSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [firstSeries]
          $ allAxises ⊝ firstSeries
        firstAggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelectWithNone
        secondAggregation =
          setPreviousValueFrom (index current.aggregations 1) aggregationSelectWithNone
        thirdAggregation =
          setPreviousValueFrom (index current.aggregations 2) aggregationSelectWithNone
    in { series: [firstSeries, secondSeries]
       , dimensions: []
       , measures: [firstMeasures, secondMeasures, thirdMeasures]
       , aggregations: [firstAggregation, secondAggregation]
       }

  radarConfiguration ∷ Axes → ChartConfiguration → ChartConfiguration
  radarConfiguration axes current =
    let allAxises = (axes.category ⊕ axes.time ⊕ axes.value)
        dimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ axes.category
        firstMeasures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ ifSelected [dimensions]
          $ axes.value
        firstSeries =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [firstMeasures]
          $ allAxises ⊝ dimensions ⊝ firstMeasures
        secondSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [firstSeries]
          $ allAxises ⊝ dimensions ⊝ firstMeasures ⊝ firstSeries
        firstAggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
    in { series: [firstSeries, secondSeries]
       , dimensions: [dimensions]
       , measures: [firstMeasures]
       , aggregations: [firstAggregation]
       }

  funnelConfiguration ∷ Axes → ChartConfiguration → ChartConfiguration
  funnelConfiguration axes current =
    let allAxises = (axes.category ⊕ axes.time ⊕ axes.value)
        dimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ axes.category
        firstMeasures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ ifSelected [dimensions]
          $ axes.value
        firstSeries =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [firstMeasures]
          $ allAxises ⊝ dimensions ⊝ firstMeasures
        firstAggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
    in { series: [firstSeries]
       , dimensions: [dimensions]
       , measures: [firstMeasures]
       , aggregations: [firstAggregation]
       }

  heatmapConfiguration ∷ Axes → ChartConfiguration → ChartConfiguration
  heatmapConfiguration axes current =
    let allAxises = (axes.category ⊕ axes.time ⊕ axes.value)
        firstDimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ allAxises
        secondDimensions =
          setPreviousValueFrom (index current.dimensions 1)
          $ autoSelect $ newSelect $ allAxises
        measures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ ifSelected [firstDimensions, secondDimensions]
          $ axes.value ⊝ firstDimensions ⊝ secondDimensions
        series =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [firstDimensions, secondDimensions, measures]
          $ allAxises ⊝ firstDimensions ⊝ secondDimensions ⊝ measures
        aggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
    in { series: [series]
       , dimensions: [firstDimensions, secondDimensions]
       , measures: [measures]
       , aggregations: [aggregation]
       }

peek ∷ ∀ a. ChildQuery a → DSL Unit
peek _ = configure *> CC.raiseUpdatedP' CC.EvalModelUpdate
