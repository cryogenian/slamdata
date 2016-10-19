module SlamData.Workspace.Card.BuildChart.PunchCard.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.PunchCard.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.Map as M
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Global (infinity)

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL )
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.CardType.ChartType (ChartType(PunchCard))
import SlamData.Workspace.Card.BuildChart.Semantics (getMaybeString, getValues)
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.BuildChart.PunchCard.Model (PunchCardR, Model)

import Utils.Array (enumerate)

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → Ax.Axes
  → CET.CardEvalT m Port.Port
eval Nothing _ _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource axes = do
  records ← BCE.records resource
  pure $ Port.ChartInstructions (buildPunchCard conf records axes) PunchCard

type PunchCardData = (String × String) >> (Int × Number)

buildPunchCardData ∷ PunchCardR → JArray → PunchCardData
buildPunchCardData r records = map addSymbolSize aggregated
  where
  dataMap ∷ (String × String) >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ (String × String) >> Array Number
    → Json
    → (String × String) >> Array Number
  dataMapFoldFn acc js =
    let
      getValuesFromJson = getValues js
      getMaybeStringFromJson = getMaybeString js

      mbAbscissa =
        getMaybeStringFromJson r.abscissa
      mbOrdinate =
        getMaybeStringFromJson r.ordinate
      values =
        getValuesFromJson $ pure r.value

    in case mbAbscissa × mbOrdinate of
      (Just abscissa) × (Just ordinate) →
        let
          alterFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterFn Nothing =
            Just values
          alterFn (Just vs) =
            Just $ values ⊕ vs
        in M.alter alterFn (abscissa × ordinate) acc
      _ → acc

  aggregated ∷ (String × String) >> Number
  aggregated = map (Ag.runAggregation r.valueAggregation) dataMap

  minValue ∷ Number
  minValue = fromMaybe (-1.0 * infinity) $ F.minimum aggregated

  maxValue ∷ Number
  maxValue = fromMaybe infinity $ F.maximum aggregated

  distance ∷ Number
  distance = maxValue - minValue

  sizeDistance ∷ Number
  sizeDistance = r.maxSize - r.minSize

  addSymbolSize
    ∷ Number
    → Int × Number
  addSymbolSize val
    | distance ≡ zero = (Int.ceil val) × val
    | otherwise =
        (Int.ceil (r.maxSize - sizeDistance / distance * (maxValue - val))) × val


buildPunchCard ∷ PunchCardR → JArray → Ax.Axes → DSL OptionI
buildPunchCard r records axes = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.formatterItemArrayValue \{value} →
      let
        xIx = map Int.ceil $ value A.!! 0
        yIx = map Int.ceil $ value A.!! 1
        val = value A.!! 2

        xStr = foldMap ("abscissa: " ⊕ _) $ xIx >>= A.index abscissaValues
        yStr = foldMap ("<br />ordinate: " ⊕ _) $ yIx >>= A.index ordinateValues
        valStr = foldMap (("<br />value: " ⊕ _) ∘ show) val
      in
        xStr ⊕ yStr ⊕ valStr

  E.colors colors

  when r.circular $ E.polar $ pure unit

  when r.circular do
    E.angleAxis abscissaAxis
    E.radiusAxis ordinateAxis

  when (not r.circular) do
    E.xAxis abscissaAxis
    E.yAxis ordinateAxis

  E.series series

  where
  punchCardData ∷ PunchCardData
  punchCardData = buildPunchCardData r records

  abscissaAxis ∷ ∀ i. DSL (ETP.AxisI i)
  abscissaAxis = do
    E.axisType $ ET.Category
    E.disabledBoundaryGap
    E.splitLine do
      E.shown
      E.lineStyle do
        E.dashedLine
        E.color $ C.rgba 9 9 9 1.0
    E.axisLine E.hidden
    E.items $ map ET.strItem abscissaValues

  ordinateAxis ∷ ∀ i. DSL (ETP.AxisI i)
  ordinateAxis = do
    E.axisType $ ET.Category
    E.axisLine E.hidden
    E.axisLabel $ E.margin $ margin + 2
    E.items $ map ET.strItem ordinateValues

  abscissaValues ∷ Array String
  abscissaValues =
    A.sortBy (Ax.compareWithAxisType $ Ax.axisType r.abscissa axes)
      $ A.fromFoldable
      $ Set.fromFoldable
      $ map fst
      $ M.keys punchCardData

  ordinateValues ∷ Array String
  ordinateValues =
    A.sortBy (Ax.compareWithAxisType $ Ax.axisType r.ordinate axes)
      $ A.fromFoldable
      $ Set.fromFoldable
      $ map snd
      $ M.keys punchCardData

  margin ∷ Int
  margin =
    fromMaybe 6
      $ F.maximum
      $ foldMap (\((a × o) × (v × _)) → if Just a ≡ A.head abscissaValues then [v / 2] else [])
      $ M.toList punchCardData

  series = E.scatter do
    if r.circular
      then E.polarCoordinateSystem
      else E.cartesianCoordinateSystem
    E.buildItems
      $ for_ (enumerate abscissaValues) \(xIx × abscissa) →
          for_ (enumerate ordinateValues) \(yIx × ordinate) →
            for_ (M.lookup (abscissa × ordinate) punchCardData) \(symbolSize × val) → E.addItem do
              E.symbolSize symbolSize
              E.buildValues do
                E.addValue $ Int.toNumber xIx
                E.addValue $ Int.toNumber yIx
                E.addValue val
