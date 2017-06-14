module SlamData.Workspace.Card.CardType.VizType where

import SlamData.Prelude

import Data.Argonaut (fromString, class EncodeJson, class DecodeJson, decodeJson)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data InputType
  = Text
  | Numeric
  | Date
  | Time
  | Datetime

allInputTypes ∷ Array InputType
allInputTypes = [ Text, Numeric, Date, Time, Datetime ]

derive instance eqInputType ∷ Eq InputType
derive instance ordInputType ∷ Ord InputType

data SelectType
  = Dropdown
  | Radio
  | Checkbox

allSelectTypes ∷ Array SelectType
allSelectTypes = [ Dropdown, Radio, Checkbox ]

derive instance eqSelectType ∷ Eq SelectType
derive instance ordSelectType ∷ Ord SelectType

data GeoType
  = GeoMarker
  | GeoHeatmap

allGeoTypes ∷ Array GeoType
allGeoTypes = [ GeoMarker, GeoHeatmap ]

derive instance eqGeoType ∷ Eq GeoType
derive instance ordGeoType ∷ Ord GeoType

data ChartType
  = Pie
  | Line
  | Bar
  | Area
  | Scatter
  | Radar
  | Funnel
  | Graph
  | Heatmap
  | Sankey
  | Gauge
  | Boxplot
  | PunchCard
  | Candlestick
  | Parallel

allChartTypes ∷ Array ChartType
allChartTypes =
  [ Pie
  , Line
  , Bar
  , Area
  , Scatter
  , Radar
  , Funnel
  , Graph
  , Heatmap
  , Sankey
  , Gauge
  , Boxplot
  , PunchCard
  , Candlestick
  , Parallel
  ]

derive instance eqChartType ∷ Eq ChartType
derive instance ordChartType ∷ Ord ChartType

data VizType
  = Metric
  | PivotTable
  | Input InputType
  | Select SelectType
  | Geo GeoType
  | Chart ChartType

miscVizTypes ∷ Array VizType
miscVizTypes = [ Metric, PivotTable ]

print ∷ VizType → String
print = case _ of
  Metric → "metric"
  PivotTable → "pivot-table"
  Geo geo → case geo of
    GeoMarker → "geo-marker"
    GeoHeatmap → "geo-heatmap"
  Select st → case st of
    Dropdown → "dropdown"
    Radio → "radio"
    Checkbox → "checkbox"
  Input it → case it of
    Text → "text"
    Numeric → "numeric"
    Date → "date"
    Datetime → "datetime"
    Time → "time"
  Chart ct → case ct of
    Pie → "pie"
    Line → "line"
    Bar → "bar"
    Area → "area"
    Scatter → "scatter"
    Radar → "radar"
    Funnel → "funnel"
    Graph → "graph"
    Heatmap → "heatmap"
    Sankey → "sankey"
    Gauge → "gauge"
    Boxplot → "boxplot"
    PunchCard → "punchcard"
    Candlestick → "candlestick"
    Parallel → "parallel"

parse ∷ String → String ⊹ VizType
parse = case _ of
  "metric" → Right Metric
  "pivot-table" → Right PivotTable
  "geo-marker" → Right $ Geo GeoMarker
  "geo-heatmap" → Right $ Geo GeoHeatmap
  "dropdown" → Right $ Select Dropdown
  "radio" → Right $ Select Radio
  "checkbox" → Right $ Select Checkbox
  "text" → Right $ Input Text
  "numeric" → Right $ Input Numeric
  "date" → Right $ Input Date
  "datetime" → Right $ Input Datetime
  "time" → Right $ Input Time
  "pie" → Right $ Chart Pie
  "line" → Right $ Chart Line
  "bar" → Right $ Chart Bar
  "area" → Right $ Chart Area
  "scatter" → Right $ Chart Scatter
  "radar" → Right $ Chart Radar
  "funnel" → Right $ Chart Funnel
  "graph" → Right $ Chart Graph
  "heatmap" → Right $ Chart Heatmap
  "sankey" → Right $ Chart Sankey
  "gauge" → Right $ Chart Gauge
  "boxplot" → Right $ Chart Boxplot
  "punchcard" → Right $ Chart PunchCard
  "candlestick" → Right $ Chart Candlestick
  "parallel" → Right $ Chart Parallel
  s → Left $ "The \"" <> s <> "\" is incorrect vizType"

all ∷ Array VizType
all =
  miscVizTypes
  <> map Geo allGeoTypes
  <> map Input allInputTypes
  <> map Select allSelectTypes
  <> map Chart allChartTypes

derive instance eqVizType ∷ Eq VizType
derive instance ordVizType ∷ Ord VizType

instance encodeJsonVizType ∷ EncodeJson VizType where
  encodeJson = fromString ∘ print

instance decodeJsonVizType ∷ DecodeJson VizType where
  decodeJson = decodeJson >=> parse

instance arbitraryVizType ∷ SC.Arbitrary VizType where
  arbitrary = Gen.allInArray all

name ∷ VizType → String
name = case _ of
  Metric → "Metric"
  PivotTable → "Pivot Table"
  Geo geo → case geo of
    GeoMarker → "Marker"
    GeoHeatmap → "Heatmap"
  Select st → case st of
    Dropdown → "Dropdown"
    Radio → "Radio"
    Checkbox → "Checkbox"
  Input it → case it of
    Text → "Text Input"
    Numeric → "Numeric Input"
    Date → "Date Input"
    Datetime → "Datetime Input"
    Time → "Time Input"
  Chart ct → case ct of
    Pie → "Pie"
    Line → "Line"
    Bar → "Bar"
    Area → "Area"
    Scatter → "Scatter"
    Radar → "Radar"
    Funnel → "Funnel"
    Graph → "Graph"
    Heatmap → "Heatmap"
    Sankey → "Sankey"
    Gauge → "Gauge"
    Boxplot → "Boxplot"
    PunchCard → "Punch Card"
    Candlestick → "Candlestick"
    Parallel → "Parallel"

lightIconSrc ∷ VizType → String
lightIconSrc vt = "img/viz/light/" <> print vt <> ".svg"

darkIconSrc ∷ VizType → String
darkIconSrc vt = "img/viz/dark/" <> print vt <> ".svg"
