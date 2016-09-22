module SlamData.Workspace.Card.BuildChart.Pie.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Pie.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Lens ((^?))
import Data.Map as M
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Pie.Model (Model, PieR)
import SlamData.Workspace.Card.BuildChart.Common.Positioning (adjustRadialPositions, adjustDonutRadiuses, RadialPosition, WithDonutRadius, radialTitles)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Pie))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port


eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  records ← BCE.records resource
  pure $ Port.ChartInstructions (buildPie conf records) Pie


type OnePieSeries =
  RadialPosition
  ( series ∷ Array DonutSeries
  , name ∷ Maybe String
  )

type DonutSeries =
  WithDonutRadius
  ( name ∷ Maybe String
  , items ∷ String >> Number
  )

buildPieData ∷ PieR → JArray → Array OnePieSeries
buildPieData r records = series
  where
  -- | maybe parallel >> maybe donut >> category name >> values
  dataMap ∷ Maybe String >> Maybe String >> String >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> Maybe String >> String >> Array Number
    → Json
    → Maybe String >> Maybe String >> String >> Array Number
  dataMapFoldFn acc js =
    case toString =<< cursorGet r.category js of
      Nothing → acc
      Just categoryKey →
        let
          mbParallel = toString =<< flip cursorGet js =<< r.parallel
          mbDonut = toString =<< flip cursorGet js =<< r.donut
          values = foldMap A.singleton $ toNumber =<< cursorGet r.value js

          alterParallelFn
            ∷ Maybe (Maybe String >> String >> Array Number)
            → Maybe (Maybe String >> String >> Array Number)
          alterParallelFn Nothing =
            Just $ M.singleton mbDonut $ M.singleton categoryKey values
          alterParallelFn (Just donut) =
            Just $ M.alter alterDonutFn mbDonut donut

          alterDonutFn
            ∷ Maybe (String >> Array Number)
            → Maybe (String >> Array Number)
          alterDonutFn Nothing =
            Just $ M.singleton categoryKey values
          alterDonutFn (Just category) =
            Just $ M.alter alterCategoryFn categoryKey category

          alterCategoryFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterCategoryFn Nothing = Just values
          alterCategoryFn (Just arr) = Just $ arr ⊕ values
        in
          M.alter alterParallelFn mbParallel acc


  rawSeries ∷ Array OnePieSeries
  rawSeries =
    foldMap mkOnePieSeries $ M.toList dataMap

  mkOnePieSeries
    ∷ Maybe String × (Maybe String >> String >> Array Number)
    → Array OnePieSeries
  mkOnePieSeries (name × donutSeries) =
    [{ name
     , x: Nothing
     , y: Nothing
     , radius: Nothing
     , series: foldMap mkDonutSeries $ M.toList donutSeries
     }]

  mkDonutSeries
    ∷ Maybe String × (String >> Array Number)
    → Array DonutSeries
  mkDonutSeries (name × items) =
    [{ name
     , radius: Nothing
     , items: map (Ag.runAggregation r.valueAggregation) items
     }]

  series ∷ Array OnePieSeries
  series = map (\x → x{series = adjustDonutRadiuses x.series}) $ adjustRadialPositions rawSeries

buildPie ∷ PieR → JArray → DSL OptionI
buildPie r records = do
  E.tooltip E.triggerItem

  E.colors colors

  E.legend do
    E.leftLeft
    E.textStyle do
      E.fontSize 12
      E.fontFamily "Ubuntu, sans"
    E.orient ET.Vertical
    E.items $ map ET.strItem legendNames

  E.series series

  radialTitles pieData

  where
  pieData ∷ Array OnePieSeries
  pieData = buildPieData r records

  legendNames ∷ Array String
  legendNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.items
                            ⋙ M.keys
                            ⋙ Set.fromFoldable)
                )
        pieData

  series ∷ ∀ i. DSL (pie ∷ ETP.I|i)
  series = for_ pieData \{x, y, radius: parallelR, series} →
    for_ series \{radius, items, name} → E.pie do
      E.buildCenter do
        traverse_ (E.setX ∘ E.percents) x
        traverse_ (E.setY ∘ E.percents) y

      for_ parallelR \pR →
        for_ radius \{start, end} → E.buildRadius do
          E.setStart $ E.percents $ start * pR
          E.setEnd $ E.percents $ end * pR

      for_ name E.name

      E.buildItems $ for_ (M.toList $ items) \(key × value) →
        E.addItem do
          E.value value
          E.name key