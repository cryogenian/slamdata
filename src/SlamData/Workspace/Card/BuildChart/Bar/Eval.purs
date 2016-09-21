module SlamData.Workspace.Card.BuildChart.Bar.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Bar.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
import Data.Map as M
import Data.Int as Int
import Data.Set as Set
import Data.String as Str

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Math as Math

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Bar.Model (Model, BarR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Bar))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port

import Utils.DOM (getTextWidthPure)

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → Axes
  → CET.CardEvalT m Port.Port
eval Nothing _ _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource axes = do
  records ← BCE.records resource
  pure $ Port.ChartInstructions (buildBar conf records axes) Bar


type BarSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

type BarStacks =
  { stack ∷ Maybe String
  , series ∷ Array BarSeries
  }

buildBarData ∷ BarR → JArray → Array BarStacks
buildBarData r records = series
  where
  -- | maybe stack >> maybe parallel >> category >> values
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
          mbStack = toString =<< flip cursorGet js =<< r.stack
          mbParallel = toString =<< flip cursorGet js =<< r.parallel
          values = foldMap A.singleton $ toNumber =<< cursorGet r.value js

          alterStackFn
            ∷ Maybe (Maybe String >> String >> Array Number)
            → Maybe (Maybe String >> String >> Array Number)
          alterStackFn Nothing =
            Just $ M.singleton mbParallel $ M.singleton categoryKey values
          alterStackFn (Just parallel) =
            Just $ M.alter alterParallelFn mbParallel parallel

          alterParallelFn
            ∷ Maybe (String >> Array Number)
            → Maybe (String >> Array Number)
          alterParallelFn Nothing =
            Just $ M.singleton categoryKey values
          alterParallelFn (Just category) =
            Just $ M.alter alterCategoryFn categoryKey category

          alterCategoryFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterCategoryFn Nothing = Just values
          alterCategoryFn (Just arr) = Just $ arr ⊕ values
        in
          M.alter alterStackFn mbStack acc

  series ∷ Array BarStacks
  series =
    foldMap mkBarStack $ M.toList dataMap

  mkBarStack
    ∷ Maybe String × (Maybe String >> String >> Array Number)
    → Array BarStacks
  mkBarStack (stack × sers) =
    [{ stack
     , series: foldMap mkBarSeries $ M.toList sers
     }]

  mkBarSeries
    ∷ Maybe String × (String >> Array Number)
    → Array BarSeries
  mkBarSeries (name × items) =
    [{ name
     , items: map (Ag.runAggregation r.valueAggregation) items
     }]


buildBar ∷ BarR → JArray → Axes → DSL OptionI
buildBar r records axes = do
  E.tooltip E.triggerAxis

  E.colors colors

  E.xAxis do
    E.axisType ET.Category
    E.items $ map ET.strItem xValues
    when xAxisIsCategory $ E.interval 0
    E.axisLabel do
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontSize r.axisLabelFontSize
        E.fontFamily "Ubuntu, sans"

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    unless (xAxisIsCategory ∧ A.length seriesNames > 40) E.hidden
    E.items $ map ET.strItem seriesNames
    E.leftLeft
    E.topBottom

  E.grid $ E.bottomPx labelHeight

  E.series series

  where

  barData ∷ Array BarStacks
  barData = buildBarData r records

  xAxisIsCategory ∷ Boolean
  xAxisIsCategory = F.elem r.category axes.category


  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (_.series ⋙ foldMap (_.name ⋙ foldMap Set.singleton ))
        barData

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.items
                            ⋙ M.keys
                            ⋙ Set.fromFoldable)) barData
  -- TODO: use semantics
  xSortFn = compare

  labelHeight ∷ Int
  labelHeight =
    let
      longest =
        fromMaybe ""
          $ F.maximumBy (\a b → compare (Str.length a) (Str.length b)) xValues

      width =
        getTextWidthPure longest
          $ "normal " ⊕ show r.axisLabelFontSize ⊕ "px Ubuntu"

      minHeight = 24.0

    in
      Int.round
        $ add minHeight
        $ max (Int.toNumber r.axisLabelFontSize + 2.0)
        $ Math.abs
        $ width
        * Math.sin (r.axisLabelAngle / 180.0 * Math.pi)

  series ∷ ∀ i. DSL (bar ∷ ETP.I|i)
  series = for_ barData \stacked →
    for_ stacked.series \serie → E.bar do
      E.buildItems $ for_ xValues \key →
        case M.lookup key serie.items of
          Nothing → E.missingItem
          Just v → E.addItem $ E.value v
      for_ stacked.stack E.stack
      for_ serie.name E.name
