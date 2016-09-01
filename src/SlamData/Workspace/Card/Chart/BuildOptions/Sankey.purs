module SlamData.Workspace.Card.Chart.BuildOptions.Sankey where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Chart.Semantics as Sem

type SankeyR =
  { source ∷ JCursor
  , target ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , axes ∷ Axes
  }

type SankeyItem =
  { source ∷ String
  , target ∷ String
  , weight ∷ Number
  }

type SankeyData = Array SankeyItem

buildSankeyData ∷ JArray → SankeyR → SankeyData
buildSankeyData records r = items
  where
  axesMap ∷ M.Map JCursor Axis
  axesMap = analyzeJArray records

  valueMap ∷ M.Map (String × String) Number
  valueMap = map (Ag.runAggregation r.valueAggregation) valueArrMap

  valueArrMap ∷ M.Map (String × String) (Array Number)
  valueArrMap = foldl valueFoldFn M.empty records

  valueFoldFn
    ∷ M.Map (String × String) (Array Number)
    → Json
    → M.Map (String × String) (Array Number)
  valueFoldFn acc js =
    let
      mbSource = toString =<< cursorGet r.source js
      mbTarget = toString =<< cursorGet r.target js
      mbValue = toNumber =<< cursorGet r.value js
    in
      case mbSource × mbTarget × mbValue of
        Just source × Just target × Just value →
          M.alter (valueAlterFn value) (source × target) acc
        _ → acc

  valueAlterFn ∷ Number → Maybe (Array Number) → Maybe (Array Number)
  valueAlterFn a Nothing = Just [ a ]
  valueAlterFn a (Just arr) = Just $ A.cons a arr

  sources ∷ Array (Maybe String)
  sources =
    foldMap (pure ∘ map Sem.printSemantics)
      $ foldMap Ax.runAxis
      $ M.lookup r.source axesMap

  targets ∷ Array (Maybe String)
  targets =
    foldMap (pure ∘ map Sem.printSemantics)
      $ foldMap Ax.runAxis
      $ M.lookup r.target axesMap


  maxLength ∷ Int
  maxLength =
    fromMaybe zero
    $ F.maximum
      [ A.length sources
      , A.length targets
      ]

  nothingTailed ∷ ∀ a. Array (Maybe a) → Array (Maybe a)
  nothingTailed heads = heads ⊕ do
    guard (maxLength > A.length heads)
    map (const Nothing) $ A.range 0 (maxLength - A.length heads)

  items ∷ SankeyData
  items =
    A.nubBy (\r1 r2 → r1.source ≡ r2.source ∧ r1.target ≡ r2.target)
    $ A.catMaybes $ map findAndBuildItem $ A.zip (nothingTailed targets) (nothingTailed sources)

  findAndBuildItem ∷ Maybe String × Maybe String → Maybe SankeyItem
  findAndBuildItem ((Just target) × (Just source)) =
    {source, target, weight: _} <$> M.lookup (source × target) valueMap
  findAndBuildItem _ = Nothing

buildSankey ∷ SankeyR → JArray → DSL OptionI
buildSankey r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.colors colors

  E.series $ E.sankey do
    E.buildItems items
    E.buildLinks links

    E.lineStyle $ E.normal $ E.curveness 0.3
  where
  sankeyData ∷ SankeyData
  sankeyData = buildSankeyData records r

  links ∷ DSL ETP.LinksI
  links = for_ sankeyData \item → E.addLink do
    E.sourceName item.source
    E.targetName item.target
    E.value item.weight


  items ∷ DSL ETP.ItemsI
  items =
    for_
      (A.nub $ (_.source <$> sankeyData) ⊕ (_.target <$> sankeyData))
      (E.addItem ∘ E.name)
