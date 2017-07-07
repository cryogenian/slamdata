module SlamData.Workspace.Card.CardType.Chart where

import SlamData.Prelude

import Data.Variant (inj, on)

import Halogen.HTML as H

import Unsafe.Coerce (unsafeCoerce)

_pie = SProxy ∷ SProxy "pie"
_line = SProxy ∷ SProxy "line"
_bar = SProxy ∷ SProxy "bar"
_area = SProxy ∷ SProxy "area"
_scatter = SProxy ∷ SProxy "scatter"
_radar = SProxy ∷ SProxy "radar"
_funnel = SProxy ∷ SProxy "funnel"
_graph = SProxy ∷ SProxy "graph"
_heatmap = SProxy ∷ SProxy "heatmap"
_sankey = SProxy ∷ SProxy "sankey"
_gauge = SProxy ∷ SProxy "gauge"
_boxplot = SProxy ∷ SProxy "boxplot"
_metric = SProxy ∷ SProxy "metric"
_pivot = SProxy ∷ SProxy "pivot"
_punchCard = SProxy ∷ SProxy "punchCard"
_candlestick = SProxy ∷ SProxy "candlestick"
_parallel = SProxy ∷ SProxy "parallel"

type ChartR r =
  ( pie ∷ Unit
  , line ∷ Unit
  , bar ∷ Unit
  , area ∷ Unit
  , scatter ∷ Unit
  , radar ∷ Unit
  , funnel ∷ Unit
  , graph ∷ Unit
  , heatmap ∷ Unit
  , sankey ∷ Unit
  , gauge ∷ Unit
  , boxplot ∷ Unit
  , metric ∷ Unit
  , pivot ∷ Unit
  , punchCard ∷ Unit
  , candlestick ∷ Unit
  , parallel ∷ Unit
  | r)

type Chart r = Variant (ChartR r)

all ∷ ∀ r. Array (Chart r)
all =
  [ pie
  , line
  , bar
  , area
  , scatter
  , radar
  , funnel
  , graph
  , heatmap
  , sankey
  , gauge
  , boxplot
  , metric
  , pivot
  , punchCard
  , candlestick
  , parallel
  ]

pie ∷ ∀ r. Variant (pie ∷ Unit|r)
pie = inj _pie unit

line ∷ ∀ r. Variant (line ∷ Unit|r)
line = inj _line unit

bar ∷ ∀ r. Variant (bar ∷ Unit|r)
bar = inj _bar unit

area ∷ ∀ r. Variant (area ∷ Unit|r)
area = inj _area unit

scatter ∷ ∀ r. Variant (scatter ∷ Unit|r)
scatter = inj _scatter unit

radar ∷ ∀ r. Variant (radar ∷ Unit|r)
radar = inj _radar unit

funnel ∷ ∀ r. Variant (funnel ∷ Unit|r)
funnel = inj _funnel unit

graph ∷ ∀ r. Variant (graph ∷ Unit|r)
graph = inj _graph unit

heatmap ∷ ∀ r. Variant (heatmap ∷ Unit|r)
heatmap = inj _heatmap unit

sankey ∷ ∀ r. Variant (sankey ∷ Unit|r)
sankey = inj _sankey unit

gauge ∷ ∀ r. Variant (gauge ∷ Unit|r)
gauge = inj _gauge unit

boxplot ∷ ∀ r. Variant (boxplot ∷ Unit|r)
boxplot = inj _boxplot unit

metric ∷ ∀ r. Variant (metric ∷ Unit|r)
metric = inj _metric unit

pivot ∷ ∀ r. Variant (pivot ∷ Unit|r)
pivot = inj _pivot unit

punchCard ∷ ∀ r. Variant (punchCard ∷ Unit|r)
punchCard = inj _punchCard unit

candlestick ∷ ∀ r. Variant (candlestick ∷ Unit|r)
candlestick = inj _candlestick unit

parallel ∷ ∀ r. Variant (parallel ∷ Unit|r)
parallel = inj _parallel unit

eq_ ∷ ∀ r rr. (Variant r → Variant rr → Boolean) → Chart r → Chart rr → Boolean
eq_ cb r = cb (unsafeCoerce r)
  # on _pie (on _pie tt ff r)
  # on _line (on _line tt ff r)
  # on _bar (on _bar tt ff r)
  # on _area (on _area tt ff r)
  # on _scatter (on _scatter tt ff r)
  # on _radar (on _radar tt ff r)
  # on _funnel (on _funnel tt ff r)
  # on _graph (on _graph tt ff r)
  # on _heatmap (on _heatmap tt ff r)
  # on _sankey (on _sankey tt ff r)
  # on _gauge (on _gauge tt ff r)
  # on _boxplot (on _boxplot tt ff r)
  # on _metric (on _metric tt ff r)
  # on _pivot (on _pivot tt ff r)
  # on _punchCard (on _punchCard tt ff r)
  # on _candlestick (on _candlestick tt ff r)
  # on _parallel (on _parallel tt ff r)

print ∷ ∀ r. (Variant r → String) → Chart r → String
print cb = cb
  # on _pie (const "pie")
  # on _line (const "line")
  # on _bar (const "bar")
  # on _area (const "area")
  # on _scatter (const "scatter")
  # on _radar (const "radar")
  # on _funnel (const "funnel")
  # on _graph (const "graph")
  # on _heatmap (const "heatmap")
  # on _sankey (const "sankey")
  # on _gauge (const "gauge")
  # on _boxplot (const "boxplot")
  # on _metric (const "metric")
  # on _pivot (const "pivot")
  # on _punchCard (const "punch-card")
  # on _candlestick (const "candlestick")
  # on _parallel (const "parallel")

parse ∷ ∀ r. String → String ⊹ Chart r
parse = case _ of
  "pie" → Right pie
  "line" → Right line
  "bar" → Right bar
  "area" → Right area
  "scatter" → Right scatter
  "radar" → Right radar
  "funnel" → Right funnel
  "graph" → Right graph
  "heatmap" → Right heatmap
  "sankey" → Right sankey
  "gauge" → Right gauge
  "boxplot" → Right boxplot
  "metric" → Right metric
  "pivot" → Right pivot
  "punch-card" → Right punchCard
  "candlestick" → Right candlestick
  "parallel" → Right parallel
  _ → Left "this is not chart type"

name ∷ ∀ r. (Variant r → String) → Chart r → String
name cb = cb
  # on _pie (const "Pie")
  # on _line (const "Line")
  # on _bar (const "Bar")
  # on _area (const "Area")
  # on _scatter (const "Scatter")
  # on _radar (const "Radar")
  # on _funnel (const "Funnel")
  # on _graph (const "Graph")
  # on _heatmap (const "Heatmap")
  # on _sankey (const "Sankey")
  # on _gauge (const "Gauge")
  # on _boxplot (const "Boxplot")
  # on _metric (const "Metric")
  # on _pivot (const "Pivot")
  # on _punchCard (const "Punch-card")
  # on _candlestick (const "Candlestick")
  # on _parallel (const "Parallel")

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Chart r → Boolean
consumerInteractable cb = cb
  # on _pie ff
  # on _line ff
  # on _bar ff
  # on _area ff
  # on _scatter ff
  # on _radar ff
  # on _funnel ff
  # on _graph ff
  # on _heatmap ff
  # on _sankey ff
  # on _gauge ff
  # on _boxplot ff
  # on _metric ff
  # on _pivot ff
  # on _punchCard ff
  # on _candlestick ff
  # on _parallel ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Chart r → Array H.ClassName
cardClasses cb = cb
  # on _pie clss
  # on _line clss
  # on _bar clss
  # on _area clss
  # on _scatter clss
  # on _radar clss
  # on _funnel clss
  # on _graph clss
  # on _heatmap clss
  # on _sankey clss
  # on _gauge clss
  # on _boxplot clss
  # on _metric clss
  # on _pivot clss
  # on _punchCard clss
  # on _candlestick clss
  # on _parallel clss
  where
  clss _ = [ H.ClassName "sd-card-chart-options" ]
