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

module SlamData.Workspace.Card.Component.State
  ( CardState(..)
  , initialCardState
  , _element
  , _breaker
  , _pending
  , AnyCardState
  , _AceState
  , _MarkdownState
  , _SearchState
  , _TableState
  , _ChartState
  , _DownloadState
  , _VariablesState
  , _TroubleshootState
  , _NextState
  , _CacheState
  , _OpenState
  , _DownloadOptionsState
  , _DraftboardState
  , _ErrorState
  , _PendingState
  , _BuildMetricState
  , _BuildSankeyState
  , _BuildGaugeState
  , _BuildGraphState
  , _BuildPieState
  , _BuildBarState
  , _BuildLineState
  , _BuildAreaState
  , _BuildScatterState
  , _BuildRadarState
  , _BuildPivotTableState
  , _BuildFunnelState
  , _BuildBoxplotState
  , _BuildHeatmapState
  , _BuildPunchCardState
  , _BuildCandlestickState
  , _BuildParallelState
  , _SetupDropdownState
  , _SetupRadioState
  , _SetupCheckboxState
  , _SetupTextState
  , _SetupNumericState
  , _SetupDateState
  , _SetupTimeState
  , _SetupDatetimeState
  , _SetupStaticState
  , _FormInputState
  , _TabsState
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.EventLoop (Breaker)

import Data.Lens (Lens', lens, Prism', prism')

import DOM.HTML.Types (HTMLElement)

import SlamData.Workspace.Card.Ace.Component.State as Ace
import SlamData.Workspace.Card.Troubleshoot.Component.State as Troubleshoot
import SlamData.Workspace.Card.Cache.Component.State as Cache
import SlamData.Workspace.Card.Chart.Component.State as Chart
import SlamData.Workspace.Card.Download.Component.State as Download
import SlamData.Workspace.Card.DownloadOptions.Component.State as DOpts
import SlamData.Workspace.Card.Draftboard.Component.State as Draftboard
import SlamData.Workspace.Card.Error.Component.State as Error
import SlamData.Workspace.Card.Pending.Component.State as Pending
import SlamData.Workspace.Card.Table.Component.State as Table
import SlamData.Workspace.Card.Markdown.Component.State as Markdown
import SlamData.Workspace.Card.Next.Component.State as Next
import SlamData.Workspace.Card.Open.Component.State as Open
import SlamData.Workspace.Card.Search.Component.State as Search
import SlamData.Workspace.Card.Setups.Chart.Metric.Component.State as BuildMetric
import SlamData.Workspace.Card.Setups.Chart.Sankey.Component.State as BuildSankey
import SlamData.Workspace.Card.Setups.Chart.Gauge.Component.State as BuildGauge
import SlamData.Workspace.Card.Setups.Chart.Graph.Component.State as BuildGraph
import SlamData.Workspace.Card.Setups.Chart.Pie.Component.State as BuildPie
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.State as BuildBar
import SlamData.Workspace.Card.Setups.Chart.Line.Component.State as BuildLine
import SlamData.Workspace.Card.Setups.Chart.Area.Component.State as BuildArea
import SlamData.Workspace.Card.Setups.Chart.Scatter.Component.State as BuildScatter
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.State as BuildFunnel
import SlamData.Workspace.Card.Setups.Chart.Radar.Component.State as BuildRadar
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.State as BuildPivotTable
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Component.State as BuildBoxplot
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.State as BuildHeatmap
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Component.State as BuildPunchCard
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.State as BuildCandlestick
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.State as BuildParallel
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Component.State as SetupLabeled
import SlamData.Workspace.Card.Setups.FormInput.Static.Component.State as SetupStatic
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.State as SetupTextLike
import SlamData.Workspace.Card.FormInput.Component.State as FormInput
import SlamData.Workspace.Card.Tabs.Component.State as Tabs

-- | The common state value for deck cards.
type CardState =
  { element ∷ Maybe HTMLElement
  , breaker ∷ Maybe (Breaker Unit)
  , pending ∷ Boolean
  , bus ∷ Maybe (BusRW Card.EvalMessage)
  }

-- | Creates an initial `CardState` value for an editor card.
initialCardState ∷ CardState
initialCardState =
  { element: Nothing
  , breaker: Nothing
  , pending: true
  , bus: Nothing
  }

_element ∷ Lens' CardState (Maybe HTMLElement)
_element = lens _.element _{element = _}

_breaker ∷ Lens' CardState (Maybe (Breaker Unit))
_breaker = lens _.breaker _{breaker = _}

_pending ∷ Lens' CardState Boolean
_pending = lens _.pending _{pending = _}

data AnyCardState
  = AceState Ace.State
  | MarkdownState Markdown.State
  | SearchState Search.State
  | TableState Table.State
  | ChartState Chart.State
  | DownloadState Download.State
  | VariablesState Unit
  | TroubleshootState Troubleshoot.State
  | NextState Next.State
  | CacheState Cache.State
  | OpenState Open.State
  | DownloadOptionsState DOpts.State
  | DraftboardState Draftboard.State
  | ErrorState Error.State
  | PendingState Pending.State
  | BuildMetricState BuildMetric.State
  | BuildSankeyState BuildSankey.State
  | BuildGaugeState BuildGauge.State
  | BuildGraphState BuildGraph.State
  | BuildPieState BuildPie.State
  | BuildBarState BuildBar.State
  | BuildLineState BuildLine.State
  | BuildAreaState BuildArea.State
  | BuildScatterState BuildScatter.State
  | BuildFunnelState BuildFunnel.State
  | BuildRadarState BuildRadar.State
  | BuildPivotTableState BuildPivotTable.State
  | BuildBoxplotState BuildBoxplot.State
  | BuildHeatmapState BuildHeatmap.State
  | BuildPunchCardState BuildPunchCard.State
  | BuildCandlestickState BuildCandlestick.State
  | BuildParallelState BuildParallel.State
  | SetupDropdownState SetupLabeled.State
  | SetupRadioState SetupLabeled.State
  | SetupCheckboxState SetupLabeled.State
  | SetupTextState SetupTextLike.State
  | SetupNumericState SetupTextLike.State
  | SetupDateState SetupTextLike.State
  | SetupTimeState SetupTextLike.State
  | SetupDatetimeState SetupTextLike.State
  | SetupStaticState SetupStatic.State
  | FormInputState FormInput.State
  | TabsState Tabs.State

_AceState ∷ Prism' AnyCardState Ace.State
_AceState = prism' AceState case _ of
  AceState s → Just s
  _ → Nothing

_MarkdownState ∷ Prism' AnyCardState Markdown.State
_MarkdownState = prism' MarkdownState case _ of
  MarkdownState s → Just s
  _ → Nothing

_SearchState ∷ Prism' AnyCardState Search.State
_SearchState = prism' SearchState case _ of
  SearchState s → Just s
  _ → Nothing

_TableState ∷ Prism' AnyCardState Table.State
_TableState = prism' TableState case _ of
  TableState s → Just s
  _ → Nothing

_ChartState ∷ Prism' AnyCardState Chart.State
_ChartState = prism' ChartState case _ of
  ChartState s → Just s
  _ → Nothing

_DownloadState ∷ Prism' AnyCardState Download.State
_DownloadState = prism' DownloadState case _ of
  DownloadState s → Just s
  _ → Nothing

_VariablesState ∷ Prism' AnyCardState Unit
_VariablesState = prism' VariablesState case _ of
  VariablesState s → Just s
  _ → Nothing

_TroubleshootState ∷ Prism' AnyCardState Troubleshoot.State
_TroubleshootState = prism' TroubleshootState case _ of
  TroubleshootState s → Just s
  _ → Nothing

_NextState ∷ Prism' AnyCardState Next.State
_NextState = prism' NextState case _ of
  NextState s → Just s
  _ → Nothing

_CacheState ∷ Prism' AnyCardState Cache.State
_CacheState = prism' CacheState case _ of
  CacheState s → Just s
  _ → Nothing

_OpenState ∷ Prism' AnyCardState Open.State
_OpenState = prism' OpenState case _ of
  OpenState s → Just s
  _ → Nothing

_DownloadOptionsState ∷ Prism' AnyCardState DOpts.State
_DownloadOptionsState = prism' DownloadOptionsState case _ of
  DownloadOptionsState s → Just s
  _ → Nothing

_DraftboardState ∷ Prism' AnyCardState Draftboard.State
_DraftboardState = prism' DraftboardState case _ of
  DraftboardState s → Just s
  _ → Nothing

_ErrorState ∷ Prism' AnyCardState Error.State
_ErrorState = prism' ErrorState case _ of
  ErrorState s → Just s
  _ → Nothing

_PendingState ∷ Prism' AnyCardState Pending.State
_PendingState = prism' PendingState case _ of
  PendingState s → Just s
  _ → Nothing

_BuildMetricState ∷ Prism' AnyCardState BuildMetric.State
_BuildMetricState = prism' BuildMetricState case _ of
  BuildMetricState s → Just s
  _ → Nothing

_BuildSankeyState ∷ Prism' AnyCardState BuildSankey.State
_BuildSankeyState = prism' BuildSankeyState case _ of
  BuildSankeyState s → Just s
  _ → Nothing

_BuildGaugeState ∷ Prism' AnyCardState BuildGauge.State
_BuildGaugeState = prism' BuildGaugeState case _ of
  BuildGaugeState s → Just s
  _ → Nothing

_BuildGraphState ∷ Prism' AnyCardState BuildGraph.State
_BuildGraphState = prism' BuildGraphState case _ of
  BuildGraphState s → Just s
  _ → Nothing

_BuildPieState ∷ Prism' AnyCardState BuildPie.State
_BuildPieState = prism' BuildPieState case _ of
  BuildPieState s → Just s
  _ → Nothing

_BuildBarState ∷ Prism' AnyCardState BuildBar.State
_BuildBarState = prism' BuildBarState case _ of
  BuildBarState s → Just s
  _ → Nothing

_BuildLineState ∷ Prism' AnyCardState BuildLine.State
_BuildLineState = prism' BuildLineState case _ of
  BuildLineState s → Just s
  _ → Nothing

_BuildAreaState ∷ Prism' AnyCardState BuildArea.State
_BuildAreaState = prism' BuildAreaState case _ of
  BuildAreaState s → Just s
  _ → Nothing

_BuildScatterState ∷ Prism' AnyCardState BuildScatter.State
_BuildScatterState = prism' BuildScatterState case _ of
  BuildScatterState s → Just s
  _ → Nothing

_BuildRadarState ∷ Prism' AnyCardState BuildRadar.State
_BuildRadarState = prism' BuildRadarState case _ of
  BuildRadarState s → Just s
  _ → Nothing

_BuildPivotTableState ∷ Prism' AnyCardState BuildPivotTable.State
_BuildPivotTableState = prism' BuildPivotTableState case _ of
  BuildPivotTableState s → Just s
  _ → Nothing

_BuildFunnelState ∷ Prism' AnyCardState BuildFunnel.State
_BuildFunnelState = prism' BuildFunnelState case _ of
  BuildFunnelState s → Just s
  _ → Nothing

_BuildBoxplotState ∷ Prism' AnyCardState BuildBoxplot.State
_BuildBoxplotState = prism' BuildBoxplotState case _ of
  BuildBoxplotState s → Just s
  _ → Nothing

_BuildHeatmapState ∷ Prism' AnyCardState BuildHeatmap.State
_BuildHeatmapState = prism' BuildHeatmapState case _ of
  BuildHeatmapState s → Just s
  _ → Nothing

_BuildPunchCardState ∷ Prism' AnyCardState BuildPunchCard.State
_BuildPunchCardState = prism' BuildPunchCardState case _ of
  BuildPunchCardState s → Just s
  _ → Nothing

_BuildCandlestickState ∷ Prism' AnyCardState BuildCandlestick.State
_BuildCandlestickState = prism' BuildCandlestickState case _ of
  BuildCandlestickState s → Just s
  _ → Nothing

_BuildParallelState ∷ Prism' AnyCardState BuildParallel.State
_BuildParallelState = prism' BuildParallelState case _ of
  BuildParallelState s → Just s
  _ → Nothing

_SetupDropdownState ∷ Prism' AnyCardState SetupLabeled.State
_SetupDropdownState = prism' SetupDropdownState case _ of
  SetupDropdownState s → Just s
  _ → Nothing

_SetupRadioState ∷ Prism' AnyCardState SetupLabeled.State
_SetupRadioState = prism' SetupRadioState case _ of
  SetupRadioState s → Just s
  _ → Nothing

_SetupCheckboxState ∷ Prism' AnyCardState SetupLabeled.State
_SetupCheckboxState = prism' SetupCheckboxState case _ of
  SetupCheckboxState s → Just s
  _ → Nothing

_SetupTextState ∷ Prism' AnyCardState SetupTextLike.State
_SetupTextState = prism' SetupTextState case _ of
  SetupTextState s → Just s
  _ → Nothing

_SetupNumericState ∷ Prism' AnyCardState SetupTextLike.State
_SetupNumericState = prism' SetupNumericState case _ of
  SetupNumericState s → Just s
  _ → Nothing

_SetupDateState ∷ Prism' AnyCardState SetupTextLike.State
_SetupDateState = prism' SetupDateState case _ of
  SetupDateState s → Just s
  _ → Nothing

_SetupTimeState ∷ Prism' AnyCardState SetupTextLike.State
_SetupTimeState = prism' SetupTimeState case _ of
  SetupTimeState s → Just s
  _ → Nothing

_SetupDatetimeState ∷ Prism' AnyCardState SetupTextLike.State
_SetupDatetimeState = prism' SetupDatetimeState case _ of
  SetupDatetimeState s → Just s
  _ → Nothing

_SetupStaticState ∷ Prism' AnyCardState SetupStatic.State
_SetupStaticState = prism' SetupStaticState case _ of
  SetupStaticState s → Just s
  _ → Nothing

_FormInputState ∷ Prism' AnyCardState FormInput.State
_FormInputState = prism' FormInputState case _ of
  FormInputState s → Just s
  _ → Nothing

_TabsState ∷ Prism' AnyCardState Tabs.State
_TabsState = prism' TabsState case _ of
  TabsState s → Just s
  _ → Nothing
