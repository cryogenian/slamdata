module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Viz.Model
  ) where

import SlamData.Prelude

import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Viz.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Common.Eval as BCE

eval ∷ ∀ m. BCE.ChartSetupEval ModelR m
eval = traceAny "trololo" \_ → BCE.chartSetupEval (SCC.buildBasicSql (const mempty) $ const Nothing) buildPort

buildPort ∷ ModelR → Axes → Port.Port
buildPort m axes =
  Port.ResourceKey "foo"
