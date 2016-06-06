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

module SlamData.Workspace.Card.Viz.Form.Component
  ( formComponent
  , initialState
  , Query(..)
  , QueryP
  , StateP
  , State
  , ChildState
  , ChildSlot
  , ChildQuery
  , DimensionQuery
  , SeriesQuery
  , MeasureQuery
  , DimensionSlot
  , SeriesSlot
  , MeasureSlot
  , DimensionState
  , SeriesState
  , MeasureState
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array ((!!), null, range, length)
import Data.Maybe.Unsafe (fromJust)

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.CustomProps as CP
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Form.Select (Select(..))
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), allAggregations)
import SlamData.Workspace.Card.Chart.ChartConfiguration (JSelect, ChartConfiguration)
--import SlamData.Workspace.Card.Viz.Form.Component.Render (gridClasses, GridClasses)
--import SlamData.Render.Common (row)
import SlamData.Render.CSS as Rc

data Query a
  = SetConfiguration ChartConfiguration a
  | GetConfiguration (ChartConfiguration → a)

type State = ChartConfiguration

initialState ∷ State
initialState =
  { dimensions: []
  , series: []
  , measures: []
  , aggregations: []
  }

type DimensionSlot = Int
type SeriesSlot = Int
type MeasureSlot = Int
type ChildSlot = Either DimensionSlot (Either SeriesSlot MeasureSlot)

type DimensionQuery = S.Query JCursor
type SeriesQuery = S.Query JCursor
type MeasureQuery = P.QueryP Aggregation JCursor
type MeasureAggQuery = S.Query Aggregation
type MeasureSelQuery = S.Query JCursor
type ChildQuery = Coproduct DimensionQuery (Coproduct SeriesQuery MeasureQuery)

type DimensionState = Select JCursor
type SeriesState = Select JCursor
type MeasureState = P.StateP Aggregation JCursor
type ChildState = Either DimensionState (Either SeriesState MeasureState)

type FormHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type FormDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot
type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)

cpDimension
  ∷ ChildPath
       DimensionState ChildState
       DimensionQuery ChildQuery
       DimensionSlot ChildSlot
cpDimension = cpL

cpSeries
  ∷ ChildPath
       SeriesState ChildState
       SeriesQuery ChildQuery
       SeriesSlot ChildSlot
cpSeries = cpR :> cpL

cpMeasure
  ∷ ChildPath
       MeasureState ChildState
       MeasureQuery ChildQuery
       MeasureSlot ChildSlot
cpMeasure = cpR :> cpR

formComponent ∷ H.Component StateP QueryP Slam
formComponent = H.parentComponent { render, eval, peek: Nothing }

render
  ∷ ChartConfiguration
  → FormHTML
render conf =
  HH.div
    [ HP.classes [ Rc.chartEditor ] ]
    $ fold
      [ if null conf.dimensions
          then foldMap (renderCategory 0) (conf.series !! 0)
          else foldMap (renderDimension 0) (conf.dimensions !! 0)
      , hr
      , foldMap (renderMeasure 0) (conf.measures !! 0)
      , foldMap (renderMeasure 1) (conf.measures !! 1)
      , hr
      , let ixes =
          if null conf.dimensions then { fstIx: 1, sndIx: 2} else { fstIx: 0, sndIx: 1}
        in
          foldMap (renderSeries 0) (conf.series !! ixes.fstIx)
          ⊕ foldMap (renderSeries 1) (conf.series !! ixes.sndIx)
      , hr
      ]
  where
  hr ∷ Array FormHTML
  hr = [ HH.hr_ ]
  renderDimension ∷ Int → JSelect → Array FormHTML
  renderDimension ix sel =
    [ HH.form
        [ CP.nonSubmit
        , HP.classes [ Rc.chartConfigureForm, Rc.chartDimension ]
        ]
        [ dimensionLabel
        , HH.slot' cpDimension ix \_ →
            { component: S.primarySelect (pure "Dimension")
            , initialState: sel
            }
        ]
    ]

  renderMeasure ∷ Int → JSelect → Array FormHTML
  renderMeasure ix sel =
    [ HH.form
        [ CP.nonSubmit
        , HP.classes
            [ Rc.chartConfigureForm
            , Rc.chartMeasureOne
            , Rc.withAggregation
            ]
        ]
        [ measureLabel
        , HH.slot' cpMeasure ix \_ →
            { component: childMeasure ix sel
            , initialState: H.parentState $ P.initialState aggregationSelect
            }
        ]
    ]

  renderSeries ∷  Int → JSelect → Array FormHTML
  renderSeries ix sel =
    [ HH.form
        [ CP.nonSubmit
        , HP.classes
            [ Rc.chartConfigureForm
            , Rc.chartSeriesOne
            ]
        ]
      [ seriesLabel
      , HH.slot' cpSeries ix \_ →
          { component: S.secondarySelect $ renderLabel ix "Series"
          , initialState: sel
          }
      ]
    ]

  renderCategory ∷ Int → JSelect → Array FormHTML
  renderCategory ix sel =
    [ HH.form
        [ CP.nonSubmit
        , HP.classes
            [ Rc.chartConfigureForm
            , Rc.chartCategory
            ]
        ]
        [ categoryLabel
        , HH.slot' cpSeries ix \_ →
            { component: S.primarySelect $ pure "Category"
            , initialState: sel
            }
        ]
    ]

  childMeasure i sel =
    P.selectPair
    $ if i == 0
      then { disableWhen: (_ < 2)
           , defaultWhen: (_ > 1)
           , mainState: sel
           , ariaLabel: renderLabel i "Measure"
           , classes: [Rc.aggregation, B.btnPrimary]
           }
      else { disableWhen: (_ < 1)
           , defaultWhen: (const true)
           , mainState: sel
           , ariaLabel: renderLabel i "Measure"
           , classes: [Rc.aggregation, B.btnPrimary]
           }


  aggregationSelect ∷ Select Aggregation
  aggregationSelect =
    Select
      { options: allAggregations
      , value: Just Sum
      }

  label ∷ String → FormHTML
  label str = HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text str ]

  categoryLabel ∷ FormHTML
  categoryLabel = label "Category"

  dimensionLabel ∷ FormHTML
  dimensionLabel = label "Dimension"

  measureLabel ∷ FormHTML
  measureLabel = label "Measure"

  seriesLabel ∷ FormHTML
  seriesLabel = label "Series"

  renderLabel ∷ Int → String → Maybe String
  renderLabel 0 str = pure $ "First " ⊕ str
  renderLabel 1 str = pure $ "Second " ⊕ str
  renderLabel 2 str = pure $ "Third " ⊕ str
  renderLabel n str = pure $ show n ⊕ "th " ⊕ str

eval ∷ Natural Query FormDSL
eval (SetConfiguration conf next) = do
  r ← H.get
  H.set conf
  synchronizeDimensions r.dimensions conf.dimensions
  synchronizeSeries r.series conf.series
  synchronizeMeasures r.measures conf.measures
  synchronizeAggregations r.aggregations conf.aggregations
  pure next
  where
  synchronizeDimensions ∷ Array JSelect → Array JSelect → FormDSL Unit
  synchronizeDimensions = synchronize cpDimension

  synchronizeSeries ∷ Array JSelect → Array JSelect → FormDSL Unit
  synchronizeSeries = synchronize cpSeries

  synchronize
    ∷ ∀ s
    . ChildPath s ChildState (S.Query JCursor) ChildQuery Int ChildSlot
    → Array JSelect
    → Array JSelect
    → FormDSL Unit
  synchronize prism old new =
    traverse_ (syncByIndex prism old new) $ range 0 $ getLastIndex old new

  syncByIndex
    ∷ ∀ s
    . ChildPath s ChildState (S.Query JCursor) ChildQuery Int ChildSlot
    → Array JSelect
    → Array JSelect
    → Int
    → FormDSL Unit
  syncByIndex prism old new i =
    for_ (old !! i) \_ →
      for_ (new !! i)
        $ void
        ∘ H.query' prism i
        ∘ H.action
        ∘ S.SetSelect

  synchronizeMeasures ∷ Array JSelect → Array JSelect → FormDSL Unit
  synchronizeMeasures old new =
    traverse_ (syncMeasureByIndex old new) $ range 0 $ getLastIndex old new

  syncMeasureByIndex ∷ Array JSelect → Array JSelect → Int → FormDSL Unit
  syncMeasureByIndex old new i =
    for_ (old !! i) \_ →
      for_ (new !! i)
        $ void
        ∘ H.query' cpMeasure i
        ∘ right
        ∘ H.ChildF unit
        ∘ H.action
        ∘ S.SetSelect

  synchronizeAggregations
    ∷ Array (Select Aggregation) → Array (Select Aggregation) → FormDSL Unit
  synchronizeAggregations old new =
    traverse_ (syncAggByIndex old new) $ range 0 $ getLastIndex old new

  syncAggByIndex
    ∷ Array (Select Aggregation) → Array (Select Aggregation) → Int
    → FormDSL Unit
  syncAggByIndex old new i =
    for_ (old !! i) \_ →
      for_ (new !! i)
        $ void
        ∘ H.query' cpMeasure i
        ∘ left
        ∘ H.action
        ∘ S.SetSelect

  getLastIndex ∷ ∀ a b. Array a → Array b → Int
  getLastIndex old new =
    let oldL = length old
        newL = length new
        ind = (if oldL > newL then oldL else newL) - one
    in if ind > zero then ind else zero


eval (GetConfiguration continue) = do
  conf ← H.get
  dims ← getDimensions
  series ← getSeries
  measures ← getMeasures
  r ← { dimensions: _, series: _, measures: _, aggregations: _}
    <$> getDimensions
    <*> getSeries
    <*> getMeasures
    <*> getAggregations
  pure $ continue r
  where
  getDimensions ∷ FormDSL (Array JSelect)
  getDimensions = do
    conf ← H.get
    traverse getDimension (range' 0 $ length conf.dimensions - 1)

  getDimension ∷ Int → FormDSL JSelect
  getDimension i =
    map fromJust
      $ H.query' cpDimension i
      $ H.request S.GetSelect

  getSeries ∷ FormDSL (Array JSelect)
  getSeries = do
    conf ← H.get
    traverse getSerie (range' 0 $ length conf.series - 1)

  getSerie ∷ Int → FormDSL JSelect
  getSerie i =
    map fromJust
      $ H.query' cpSeries i
      $ H.request S.GetSelect

  getMeasures ∷ FormDSL (Array JSelect)
  getMeasures = do
    conf ← H.get
    traverse getMeasure (range' 0 $ length conf.measures - 1)

  getMeasure ∷ Int → FormDSL JSelect
  getMeasure i =
    map fromJust
      $ H.query' cpMeasure i
      $ right
      $ H.ChildF unit
      $ H.request S.GetSelect

  getAggregations ∷ FormDSL (Array (Select Aggregation))
  getAggregations = do
    conf ← H.get
    traverse getAggregation (range' 0 $ length conf.measures - 1)

  getAggregation ∷ Int → FormDSL (Select Aggregation)
  getAggregation i =
    map fromJust
      $ H.query' cpMeasure i
      $ left
      $ H.request S.GetSelect

  range' ∷ Int → Int → Array Int
  range' start end =
    if end >= start
    then range start end
    else []
