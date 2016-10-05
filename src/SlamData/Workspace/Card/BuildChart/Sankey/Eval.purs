module SlamData.Workspace.Card.BuildChart.Sankey.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Sankey.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, cursorGet)
import Data.Array as A
import Data.Lens ((^?))
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Sankey.Model (Model, SankeyR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Sankey))
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
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
  pure $ Port.ChartInstructions (buildSankey conf records) Sankey

----------------------------------------------------------------------
-- SANKEY BUILDER
----------------------------------------------------------------------

type SankeyItem =
  { source ∷ String
  , target ∷ String
  , weight ∷ Number
  }

type SankeyData = Array SankeyItem

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


buildSankeyData ∷ JArray → SankeyR → SankeyData
buildSankeyData records r = items
  where
  --| source × target >> values
  dataMap ∷ (String × String) >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ M.Map (String × String) (Array Number)
    → Json
    → M.Map (String × String) (Array Number)
  dataMapFoldFn acc js =
    let
      mbSource =
        map Sem.printSemantics $ Sem.analyzeJson =<< cursorGet r.source js
      mbTarget =
        map Sem.printSemantics $ Sem.analyzeJson =<< cursorGet r.target js
      values =
        foldMap A.singleton
          $ Sem.semanticsToNumber =<< Sem.analyzeJson =<< cursorGet r.value js

      alterFn ∷ Maybe (Array Number) → Maybe (Array Number)
      alterFn Nothing = Just values
      alterFn (Just arr) = Just $ arr ⊕ values
    in
      case mbSource × mbTarget of
        Just source × Just target →
          M.alter alterFn (source × target) acc
        _ → acc

  items ∷ SankeyData
  items =
    foldMap mkItem $ M.toList dataMap

  mkItem ∷ (String × String) × Array Number → Array SankeyItem
  mkItem ((source × target) × values) =
    [ { source
      , target
      , weight: Ag.runAggregation r.valueAggregation values
      } ]
