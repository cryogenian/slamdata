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

module SlamData.Workspace.Card.Eval
  ( runCard
  , modelToEval
  , module SlamData.Workspace.Card.Eval.Transition
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Writer.Class (class MonadTell)

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Workspace.Card.BuildChart.Area.Eval as BuildArea
import SlamData.Workspace.Card.BuildChart.Bar.Eval as BuildBar
import SlamData.Workspace.Card.BuildChart.Boxplot.Eval as BuildBoxplot
import SlamData.Workspace.Card.BuildChart.Candlestick.Eval as BuildCandlestick
import SlamData.Workspace.Card.BuildChart.Funnel.Eval as BuildFunnel
import SlamData.Workspace.Card.BuildChart.Gauge.Eval as BuildGauge
import SlamData.Workspace.Card.BuildChart.Graph.Eval as BuildGraph
import SlamData.Workspace.Card.BuildChart.Heatmap.Eval as BuildHeatmap
import SlamData.Workspace.Card.BuildChart.Line.Eval as BuildLine
import SlamData.Workspace.Card.BuildChart.Metric.Eval as BuildMetric
import SlamData.Workspace.Card.BuildChart.Parallel.Eval as BuildParallel
import SlamData.Workspace.Card.BuildChart.Pie.Eval as BuildPie
import SlamData.Workspace.Card.BuildChart.PivotTable.Eval as BuildPivotTable
import SlamData.Workspace.Card.BuildChart.PunchCard.Eval as BuildPunchCard
import SlamData.Workspace.Card.BuildChart.Radar.Eval as BuildRadar
import SlamData.Workspace.Card.BuildChart.Sankey.Eval as BuildSankey
import SlamData.Workspace.Card.BuildChart.Scatter.Eval as BuildScatter
import SlamData.Workspace.Card.Cache.Eval as Cache
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.Transition (Eval(..), tagEval)
import SlamData.Workspace.Card.Markdown.Eval as MDE
import SlamData.Workspace.Card.Model as Model
import SlamData.Workspace.Card.Open.Eval as Open
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Query.Eval as Query
import SlamData.Workspace.Card.Search.Eval as Search
import SlamData.Workspace.Card.SetupFormInput.Labeled.Eval as SetupLabeled
import SlamData.Workspace.Card.SetupFormInput.TextLike.Eval as SetupTextLike
import SlamData.Workspace.Card.SetupFormInput.Static.Eval as SetupStatic
import SlamData.Workspace.Card.Variables.Eval as VariablesE
import SlamData.Workspace.Card.FormInput.Eval as FormInput


runCard
  ∷ ∀ f m
  . ( MonadAff SlamDataEffects m
    , QuasarDSL m
    , Parallel f m
    , Monad m
    )
  ⇒ CEM.CardEnv
  → CEM.CardState
  → Port.Port
  → Eval
  → m (CEM.CardResult Port.Port)
runCard env state input trans =
  CEM.runCardEvalM env state (evalCard input trans ∷ CEM.CardEval Port.Port)

evalCard
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ Port.Port
  → Eval
  → m Port.Port
evalCard = flip case _, _ of
  Error msg, _ →
    pure $ Port.CardError msg
  Pass, Port.Initial →
    CEM.throw "Cannot pass initial port"
  Pass, Port.Terminal →
    CEM.throw "Cannot pass terminal port"
  Pass, port →
    pure port
  Draftboard, _ →
    pure Port.Draftboard
  Query sql, Port.VarMap varMap →
    Port.TaggedResource <$> Query.evalQuery varMap sql
  Query sql, _ →
    Port.TaggedResource <$> Query.evalQuery Port.emptyVarMap sql
  Markdown txt, port →
    MDE.evalMarkdown port txt
  MarkdownForm model, Port.SlamDown doc →
    Port.VarMap <$> MDE.evalMarkdownForm doc model
  Search query, Port.TaggedResource { resource } →
    Port.TaggedResource <$> Search.evalSearch query resource
  Cache pathString, port@Port.TaggedResource { resource, varMap } →
    Port.TaggedResource <$> Cache.eval port pathString resource varMap
  Open res, _ →
    Port.TaggedResource <$> Open.evalOpen res
  Variables model, _ → do
    Port.VarMap <$> VariablesE.eval model
  DownloadOptions { compress, options }, Port.TaggedResource { resource } →
    pure $ Port.DownloadOptions { resource, compress, options }
  BuildMetric model, Port.TaggedResource tr →
    BuildMetric.eval tr model
  BuildSankey model, Port.TaggedResource tr →
    BuildSankey.eval tr model
  BuildGauge model, Port.TaggedResource tr →
    BuildGauge.eval tr model
  BuildGraph model, Port.TaggedResource tr →
    BuildGraph.eval tr model
  BuildPie model, Port.TaggedResource tr →
    BuildPie.eval tr model
  BuildRadar model, Port.TaggedResource tr →
    BuildRadar.eval tr model
  BuildArea model, Port.TaggedResource tr →
    BuildArea.eval tr model
  BuildLine model, Port.TaggedResource tr →
    BuildLine.eval tr model
  BuildBar model, Port.TaggedResource tr →
    BuildBar.eval tr model
  BuildScatter model, Port.TaggedResource tr →
    BuildScatter.eval tr model
  BuildFunnel model, Port.TaggedResource tr →
    BuildFunnel.eval tr model
  BuildHeatmap model, Port.TaggedResource tr →
    BuildHeatmap.eval tr model
  BuildBoxplot model, Port.TaggedResource tr →
    BuildBoxplot.eval tr model
  BuildPivotTable model, Port.TaggedResource tr →
    BuildPivotTable.eval tr model
  BuildPunchCard model, Port.TaggedResource tr →
    BuildPunchCard.eval tr model
  BuildCandlestick model, Port.TaggedResource tr →
    BuildCandlestick.eval tr model
  BuildParallel model, Port.TaggedResource tr →
    BuildParallel.eval tr model
  Chart, Port.Metric {taggedResource} →
    pure $ Port.TaggedResource taggedResource
  Chart, Port.PivotTable {taggedResource} →
    pure $ Port.TaggedResource taggedResource
  Chart, Port.ChartInstructions {taggedResource} →
    pure $ Port.TaggedResource taggedResource
  SetupCheckbox model, Port.TaggedResource tr →
    SetupLabeled.eval model tr CT.Checkbox
  SetupRadio model, Port.TaggedResource tr →
    SetupLabeled.eval model tr CT.Radio
  SetupDropdown model, Port.TaggedResource tr →
    SetupLabeled.eval model tr CT.Dropdown
  SetupText model, Port.TaggedResource tr →
    SetupTextLike.eval model tr CT.Text
  SetupNumeric model, Port.TaggedResource tr →
    SetupTextLike.eval model tr CT.Numeric
  SetupDate model, Port.TaggedResource tr →
    SetupTextLike.eval model tr CT.Date
  SetupTime model, Port.TaggedResource tr →
    SetupTextLike.eval model tr CT.Time
  SetupDatetime model, Port.TaggedResource tr →
    SetupTextLike.eval model tr CT.Datetime
  SetupStatic model, Port.TaggedResource tr →
    SetupStatic.eval model tr
  FormInput (FormInput.Labeled model), Port.SetupLabeledFormInput lp →
    FormInput.evalLabeled model lp
  FormInput (FormInput.TextLike model), Port.SetupTextLikeFormInput tlp →
    FormInput.evalTextLike model tlp
  e, i →
    CEM.throw $ "Card received unexpected input type; " <> tagEval e <> " | " <> Port.tagPort i

modelToEval ∷ Model.AnyCardModel → Eval
modelToEval = case _ of
  Model.Ace CT.SQLMode model → Query model.text
  Model.Ace CT.MarkdownMode model → Markdown model.text
  Model.Markdown model → MarkdownForm model
  Model.Search txt → Search txt
  Model.Cache fp → Cache fp
  Model.Open res → Open res
  Model.Variables model → Variables model
  Model.DownloadOptions model → DownloadOptions model
  Model.Draftboard _ → Draftboard
  Model.BuildMetric model  → BuildMetric model
  Model.BuildSankey model → BuildSankey model
  Model.BuildGauge model → BuildGauge model
  Model.BuildGraph model → BuildGraph model
  Model.BuildPie model → BuildPie model
  Model.BuildRadar model → BuildRadar model
  Model.BuildArea model → BuildArea model
  Model.BuildLine model → BuildLine model
  Model.BuildBar model → BuildBar model
  Model.BuildScatter model → BuildScatter model
  Model.BuildFunnel model → BuildFunnel model
  Model.BuildHeatmap model → BuildHeatmap model
  Model.BuildBoxplot model → BuildBoxplot model
  Model.BuildPivotTable model → BuildPivotTable model
  Model.BuildPunchCard model → BuildPunchCard model
  Model.BuildCandlestick model → BuildCandlestick model
  Model.BuildParallel model → BuildParallel model
  Model.Chart _ → Chart
  Model.SetupDropdown model → SetupDropdown model
  Model.SetupStatic model → SetupStatic model
  Model.SetupText model → SetupText model
  Model.SetupNumeric model → SetupNumeric model
  Model.SetupCheckbox model → SetupCheckbox model
  Model.SetupRadio model → SetupRadio model
  Model.SetupDate model → SetupDate model
  Model.SetupTime model → SetupTime model
  Model.SetupDatetime model → SetupDatetime model
  Model.FormInput model → FormInput model
  _ → Pass
