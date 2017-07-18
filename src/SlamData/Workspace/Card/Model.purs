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

module SlamData.Workspace.Card.Model where

import SlamData.Prelude
import Data.Argonaut ((:=), (~>), (.?))
import Data.Argonaut as J
import Data.Codec.Argonaut as CA
import Data.Array as Array
import Data.Lens (Traversal', wander, Prism', prism')
import Data.List as L
import Data.Path.Pathy (fileName, runFileName)
import Data.Rational ((%))
import Data.StrMap as StrMap
import Data.Variant (on)

import SlamData.Workspace.Card.Ace.Model as Ace
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.Ace as CTA
import SlamData.Workspace.Card.DownloadOptions.Component.State as DLO
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Model as DB
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Open.Model as Open
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Query.Model as Query
import SlamData.Workspace.Card.Setups.Chart.Area.Model as BuildArea
import SlamData.Workspace.Card.Setups.Chart.Bar.Model as BuildBar
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Model as BuildBoxplot
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Model as BuildCandlestick
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model as BuildFunnel
import SlamData.Workspace.Card.Setups.Chart.Gauge.Model as BuildGauge
import SlamData.Workspace.Card.Setups.Chart.Graph.Model as BuildGraph
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Model as BuildHeatmap
import SlamData.Workspace.Card.Setups.Chart.Line.Model as BuildLine
import SlamData.Workspace.Card.Setups.Chart.Metric.Model as BuildMetric
import SlamData.Workspace.Card.Setups.Chart.Parallel.Model as BuildParallel
import SlamData.Workspace.Card.Setups.Chart.Pie.Model as BuildPie
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as BuildPivotTable
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Model as BuildPunchCard
import SlamData.Workspace.Card.Setups.Chart.Radar.Model as BuildRadar
import SlamData.Workspace.Card.Setups.Chart.Sankey.Model as BuildSankey
import SlamData.Workspace.Card.Setups.Chart.Scatter.Model as BuildScatter
import SlamData.Workspace.Card.Setups.FormInput.Checkbox.Model as SetupCheckbox
import SlamData.Workspace.Card.Setups.FormInput.Date.Model as SetupDate
import SlamData.Workspace.Card.Setups.FormInput.Datetime.Model as SetupDatetime
import SlamData.Workspace.Card.Setups.FormInput.Dropdown.Model as SetupDropdown
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Model as SetupLabeled
import SlamData.Workspace.Card.Setups.FormInput.Numeric.Model as SetupNumeric
import SlamData.Workspace.Card.Setups.FormInput.Radio.Model as SetupRadio
import SlamData.Workspace.Card.Setups.FormInput.Static.Model as SetupStatic
import SlamData.Workspace.Card.Setups.FormInput.Text.Model as SetupText
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Model as SetupTextLike
import SlamData.Workspace.Card.Setups.FormInput.Time.Model as SetupTime
import SlamData.Workspace.Card.Setups.Geo.Heatmap.Model as SetupGeoHeatmap
import SlamData.Workspace.Card.Setups.Geo.Marker.Model as SetupGeoMarker
import SlamData.Workspace.Card.StructureEditor.Model as StructureEditor
import SlamData.Workspace.Card.Table.Model as JT
import SlamData.Workspace.Card.Tabs.Model as Tabs
import SlamData.Workspace.Card.Variables.Model as Variables
import SlamData.Workspace.Card.Viz.Model as Viz
import SlamData.Workspace.Deck.DeckId (DeckId)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

import Utils (decodec)

data AnyCardModel
  = Ace (CT.Ace ()) Ace.Model
  | Search String
  | Markdown MD.Model
  | Table JT.Model
  | Download
  | Variables Variables.Model
  | Troubleshoot
  | Cache (Maybe String)
  | Open Open.Model
  | DownloadOptions DLO.State
  | Draftboard DB.Model
  | BuildMetric BuildMetric.Model
  | BuildSankey BuildSankey.Model
  | BuildGauge BuildGauge.Model
  | BuildGraph BuildGraph.Model
  | BuildPie BuildPie.Model
  | BuildBar BuildBar.Model
  | BuildLine BuildLine.Model
  | BuildArea BuildArea.Model
  | BuildScatter BuildScatter.Model
  | BuildPivotTable BuildPivotTable.Model
  | BuildFunnel BuildFunnel.Model
  | BuildRadar BuildRadar.Model
  | BuildBoxplot BuildBoxplot.Model
  | BuildHeatmap BuildHeatmap.Model
  | BuildPunchCard BuildPunchCard.Model
  | BuildCandlestick BuildCandlestick.Model
  | BuildParallel BuildParallel.Model
  | SetupDropdown SetupDropdown.Model
  | SetupRadio SetupRadio.Model
  | SetupCheckbox SetupCheckbox.Model
  | SetupText SetupText.Model
  | SetupNumeric SetupNumeric.Model
  | SetupDate SetupDate.Model
  | SetupTime SetupTime.Model
  | SetupDatetime SetupDatetime.Model
  | SetupStatic SetupStatic.Model
  | SetupGeoMarker SetupGeoMarker.Model
  | SetupGeoHeatmap SetupGeoHeatmap.Model
  | Tabs Tabs.Model
  | StructureEditor StructureEditor.Model
  | Viz Viz.Model

instance arbitraryAnyCardModel ∷ SC.Arbitrary AnyCardModel where
  arbitrary =
    Gen.oneOf (pure Download)
      [ Ace <$> Gen.allInArray CTA.all <*> Ace.genModel
      , Search <$> SC.arbitrary
      , Markdown <$> MD.genModel
      , Table <$> JT.genModel
      , Variables <$> Variables.genModel
      , pure Troubleshoot
      , Cache <$> SC.arbitrary
      , Open <$> SC.arbitrary
      , Draftboard <$> DB.genModel
      , BuildMetric <$> BuildMetric.genModel
      , BuildSankey <$> BuildSankey.genModel
      , BuildGauge <$> BuildGauge.genModel
      , BuildGraph <$> BuildGraph.genModel
      , BuildPie <$> BuildPie.genModel
      , BuildRadar <$> BuildRadar.genModel
      , BuildBar <$> BuildBar.genModel
      , BuildLine <$> BuildLine.genModel
      , BuildArea <$> BuildArea.genModel
      , BuildScatter <$> BuildScatter.genModel
      , BuildFunnel <$> BuildFunnel.genModel
      , BuildBoxplot <$> BuildBoxplot.genModel
      , BuildHeatmap <$> BuildHeatmap.genModel
      , BuildPunchCard <$> BuildPunchCard.genModel
      , BuildCandlestick <$> BuildCandlestick.genModel
      , BuildParallel <$> BuildParallel.genModel
      , SetupDropdown <$> SetupDropdown.genModel
      , SetupRadio <$> SetupRadio.genModel
      , SetupCheckbox <$> SetupCheckbox.genModel
      , SetupText <$> SetupText.genModel
      , SetupNumeric <$> SetupNumeric.genModel
      , SetupDate <$> SetupDate.genModel
      , SetupTime <$> SetupTime.genModel
      , SetupDatetime <$> SetupDatetime.genModel
      , SetupStatic <$> SetupStatic.genModel
      , Tabs <$> Tabs.genModel
      , StructureEditor <$> StructureEditor.genModel
      , SetupGeoMarker <$> SetupGeoMarker.genModel
      , SetupGeoHeatmap <$> SetupGeoHeatmap.genModel
      , Viz <$> Viz.gen
      ]

updateCardModel ∷ AnyCardModel → AnyCardModel → AnyCardModel
updateCardModel = case _, _ of
  Markdown author, Markdown consumer →
    Markdown $ StrMap.union author consumer
  Search _, Search consumer →
    Search consumer
  Viz _, Viz consumer →
    Viz consumer
  author, _ →
    author

instance eqAnyCardModel ∷ Eq AnyCardModel where
  eq = case _, _ of
    Ace x1 y1, Ace x2 y2 → CT.eq_ (expand x1) (expand x2) && Ace.eqModel y1 y2
    Search s1, Search s2 → s1 ≡ s2
    Markdown x, Markdown y → x ≡ y
    Table x, Table y → JT.eqModel x y
    Download, Download → true
    Variables x, Variables y → Variables.eqModel x y
    Troubleshoot, Troubleshoot → true
    Cache x, Cache y → x ≡ y
    Open x, Open y → x ≡ y
    DownloadOptions x, DownloadOptions y → DLO.eqState x y
    Draftboard x, Draftboard y → DB.eqModel x y
    BuildMetric x, BuildMetric y → BuildMetric.eqModel x y
    BuildSankey x, BuildSankey y → BuildSankey.eqModel x y
    BuildGauge x, BuildGauge y → BuildGauge.eqModel x y
    BuildGraph x, BuildGraph y → BuildGraph.eqModel x y
    BuildPie x, BuildPie y → BuildPie.eqModel x y
    BuildRadar x, BuildRadar y → BuildRadar.eqModel x y
    BuildBar x, BuildBar y → BuildBar.eqModel x y
    BuildLine x, BuildLine y → BuildLine.eqModel x y
    BuildArea x, BuildArea y → BuildArea.eqModel x y
    BuildScatter x, BuildScatter y → BuildScatter.eqModel x y
    BuildFunnel x, BuildFunnel y → BuildFunnel.eqModel x y
    BuildBoxplot x, BuildBoxplot y → BuildBoxplot.eqModel x y
    BuildHeatmap x, BuildHeatmap y → BuildHeatmap.eqModel x y
    BuildPunchCard x, BuildPunchCard y → BuildPunchCard.eqModel x y
    BuildCandlestick x, BuildCandlestick y → BuildCandlestick.eqModel x y
    BuildParallel x, BuildParallel y → BuildParallel.eqModel x y
    SetupDropdown x, SetupDropdown y → SetupDropdown.eqModel x y
    SetupRadio x, SetupRadio y → SetupRadio.eqModel x y
    SetupCheckbox x, SetupCheckbox y → SetupCheckbox.eqModel x y
    SetupText x, SetupText y → SetupText.eqModel x y
    SetupNumeric x, SetupNumeric y → SetupNumeric.eqModel x y
    SetupDate x, SetupDate y → SetupDate.eqModel x y
    SetupTime x, SetupTime y → SetupTime.eqModel x y
    SetupDatetime x, SetupDatetime y → SetupDatetime.eqModel x y
    SetupStatic x, SetupStatic y → SetupStatic.eqModel x y
    Tabs x, Tabs y → Tabs.eqModel x y
    StructureEditor x, StructureEditor y → x == y
    SetupGeoMarker x, SetupGeoMarker y → SetupGeoMarker.eqModel x y
    SetupGeoHeatmap x, SetupGeoHeatmap y → SetupGeoHeatmap.eqModel x y
    Viz x, Viz y → Viz.eq_ x y
    _, _ → false

instance encodeJsonCardModel ∷ J.EncodeJson AnyCardModel where
  encodeJson = encode

instance decodeJsonCardModel ∷ J.DecodeJson AnyCardModel where
  decodeJson = decode

modelCardType ∷ AnyCardModel → CT.CardType
modelCardType = case _ of
  Ace mode _ → expand mode
  Search _ → CT.search
  BuildMetric _ → CT.metric
  BuildSankey _ → CT.sankey
  BuildGauge _ → CT.gauge
  BuildGraph _ → CT.graph
  BuildPie _ → CT.pie
  BuildRadar _ → CT.radar
  BuildBar _ → CT.bar
  BuildLine _ → CT.line
  BuildArea _ → CT.area
  BuildScatter _ → CT.scatter
  BuildPivotTable _ → CT.pivot
  BuildFunnel _ → CT.funnel
  BuildBoxplot _ → CT.boxplot
  BuildHeatmap _ → CT.heatmap
  BuildPunchCard _ → CT.punchCard
  BuildCandlestick _ → CT.candlestick
  BuildParallel _ → CT.parallel
  Markdown _ → CT.markdown
  Table _ → CT.table
  Download → CT.download
  Variables _ → CT.variables
  Troubleshoot → CT.troubleshoot
  Cache _ → CT.cache
  Open _ → CT.open
  DownloadOptions _ → CT.downloadOptions
  Draftboard _ → CT.draftboard
  SetupDropdown _ → CT.dropdown
  SetupRadio _ → CT.radio
  SetupCheckbox _ → CT.checkbox
  SetupText _ → CT.text
  SetupNumeric _ → CT.numeric
  SetupDate _ → CT.date
  SetupTime _ → CT.time
  SetupDatetime _ → CT.datetime
  SetupStatic _ → CT.static
  Tabs _ → CT.tabs
  StructureEditor _ → CT.structureEditor
  SetupGeoMarker _ → CT.geoMarker
  SetupGeoHeatmap _ → CT.geoHeatmap
  Viz _ → CT.viz

encode ∷ AnyCardModel → J.Json
encode card =
  "cardType" := (CT.encode $ modelCardType card)
    ~> "model" := encodeCardModel card
    ~> J.jsonEmptyObject

decode ∷ J.Json → Either String AnyCardModel
decode js = do
  obj ← J.decodeJson js
  cardType ← CT.decode =<< obj .? "cardType"
  model ← obj .? "model"
  decodeCardModel model cardType

encodeCardModel ∷ AnyCardModel → J.Json
encodeCardModel = case _ of
  Ace mode model → Ace.encode model
  Search txt → J.encodeJson txt
  Markdown model → MD.encode model
  Table model → CA.encode JT.codec model
  Download → J.jsonEmptyObject
  Variables model → Variables.encode model
  Troubleshoot → J.jsonEmptyObject
  Cache model → J.encodeJson model
  Open res → Open.encode res
  DownloadOptions model → CA.encode DLO.codec model
  Draftboard model → DB.encode model
  BuildMetric model → BuildMetric.encode model
  BuildSankey model → BuildSankey.encode model
  BuildGauge model → BuildGauge.encode model
  BuildGraph model → BuildGraph.encode model
  BuildPie model → BuildPie.encode model
  BuildRadar model → BuildRadar.encode model
  BuildBar model → BuildBar.encode model
  BuildLine model → BuildLine.encode model
  BuildArea model → BuildArea.encode model
  BuildScatter model → BuildScatter.encode model
  BuildPivotTable model → BuildPivotTable.encode model
  BuildFunnel model → BuildFunnel.encode model
  BuildBoxplot model → BuildBoxplot.encode model
  BuildHeatmap model → BuildHeatmap.encode model
  BuildPunchCard model → BuildPunchCard.encode model
  BuildCandlestick model → BuildCandlestick.encode model
  BuildParallel model → BuildParallel.encode model
  SetupDropdown model → SetupDropdown.encode model
  SetupRadio model → SetupRadio.encode model
  SetupCheckbox model → SetupCheckbox.encode model
  SetupText model → SetupText.encode model
  SetupNumeric model → SetupNumeric.encode model
  SetupDate model → SetupDate.encode model
  SetupTime model → SetupTime.encode model
  SetupDatetime model → SetupDatetime.encode model
  SetupStatic model → SetupStatic.encode model
  Tabs model → CA.encode Tabs.codec model
  StructureEditor model → StructureEditor.encode model
  SetupGeoMarker model → SetupGeoMarker.encode model
  SetupGeoHeatmap model → SetupGeoHeatmap.encode model
  Viz model → CA.encode Viz.codec model

decodeCardModel
  ∷ J.Json
  → CT.CardType
  → String ⊹ AnyCardModel
decodeCardModel js = case_
  # on CT._aceSql (const $ map (Ace CT.aceSql) $ Ace.decode js)
  # on CT._aceMarkdown (const $ map (Ace CT.aceMarkdown) $ Ace.decode js)
  # on CT._search (const $ map Search $ J.decodeJson js)
  # on CT._metric (const $ map BuildMetric $ BuildMetric.decode js)
  # on CT._sankey (const $ map BuildSankey $ BuildSankey.decode js)
  # on CT._gauge (const $ map BuildGauge $ BuildGauge.decode js)
  # on CT._graph (const $ map BuildGraph $ BuildGraph.decode js)
  # on CT._pie (const $ map BuildPie $ BuildPie.decode js)
  # on CT._radar (const $ map BuildRadar $ BuildRadar.decode js)
  # on CT._bar (const $ map BuildBar $ BuildBar.decode js)
  # on CT._line (const $ map BuildLine $ BuildLine.decode js)
  # on CT._area (const $ map BuildArea $ BuildArea.decode js)
  # on CT._scatter (const $ map BuildScatter $ BuildScatter.decode js)
  # on CT._pivot (const $ map BuildPivotTable $ BuildPivotTable.decode js)
  # on CT._funnel (const $ map BuildFunnel $ BuildFunnel.decode js)
  # on CT._boxplot (const $ map BuildBoxplot $ BuildBoxplot.decode js)
  # on CT._heatmap (const $ map BuildHeatmap $ BuildHeatmap.decode js)
  # on CT._punchCard (const $ map BuildPunchCard $ BuildPunchCard.decode js)
  # on CT._candlestick (const $ map BuildCandlestick $ BuildCandlestick.decode js)
  # on CT._parallel (const $ map BuildParallel $ BuildParallel.decode js)
  # on CT._dropdown (const $ map SetupDropdown $ SetupDropdown.decode js)
  # on CT._radio (const $ map SetupRadio $ SetupRadio.decode js)
  # on CT._checkbox (const $ map SetupCheckbox $ SetupCheckbox.decode js)
  # on CT._text (const $ map SetupText $ SetupText.decode js)
  # on CT._numeric (const $ map SetupNumeric $ SetupNumeric.decode js)
  # on CT._date (const $ map SetupDate $ SetupDate.decode js)
  # on CT._time (const $ map SetupTime $ SetupTime.decode js)
  # on CT._datetime (const $ map SetupDatetime $ SetupDatetime.decode js)
  # on CT._static (const $ map SetupStatic $ SetupStatic.decode js)
  # on CT._markdown (const $ map Markdown $ MD.decode js)
  # on CT._table (const $ map Table $ decodec JT.codec js)
  # on CT._download (const $ pure Download)
  # on CT._variables (const $ map Variables $ Variables.decode js)
  # on CT._troubleshoot (const $ pure Troubleshoot)
  # on CT._cache (const $ map Cache $ J.decodeJson js)
  # on CT._open (const $ map Open $ decodeOpen js)
  # on CT._downloadOptions (const $ map DownloadOptions $ decodec DLO.codec js)
  # on CT._draftboard (const $ map Draftboard $ DB.decode js)
  # on CT._tabs (const $ map Tabs $ decodec Tabs.codec js)
  # on CT._structureEditor (const $ map StructureEditor $ StructureEditor.decode js)
  # on CT._geoMarker (const $ map SetupGeoMarker $ SetupGeoMarker.decode js)
  # on CT._geoHeatmap (const $ map SetupGeoHeatmap $ SetupGeoHeatmap.decode js)
  # on CT._viz (const $ map Viz $ decodec Viz.codec js)

  where
  -- For backwards compat
  decodeOpen j =
    Open.decode j <|> (map Open.Resource <$> J.decodeJson j)

cardModelOfType ∷ Port.Out → CT.CardType → AnyCardModel
cardModelOfType (port × varMap) = case_
  # on CT._aceSql (const $ Ace CT.aceSql $ Query.initialModel port)
  # on CT._aceMarkdown (const $ Ace CT.aceMarkdown Ace.emptyModel)
  # on CT._search (const $ Search "")
  # on CT._metric (const $ BuildMetric BuildMetric.initialModel)
  # on CT._sankey (const $ BuildSankey BuildSankey.initialModel)
  # on CT._gauge (const $ BuildGauge BuildGauge.initialModel)
  # on CT._graph (const $ BuildGraph BuildGraph.initialModel)
  # on CT._pie (const $ BuildPie BuildPie.initialModel)
  # on CT._radar (const $ BuildRadar BuildRadar.initialModel)
  # on CT._bar (const $ BuildBar BuildBar.initialModel)
  # on CT._line (const $ BuildLine BuildLine.initialModel)
  # on CT._area (const $ BuildArea BuildArea.initialModel)
  # on CT._scatter (const $ BuildScatter BuildScatter.initialModel)
  # on CT._pivot (const $ BuildPivotTable BuildPivotTable.initialModel)
  # on CT._funnel (const $ BuildFunnel BuildFunnel.initialModel)
  # on CT._boxplot (const $ BuildBoxplot BuildBoxplot.initialModel)
  # on CT._heatmap (const $ BuildHeatmap BuildHeatmap.initialModel)
  # on CT._punchCard (const $ BuildPunchCard BuildPunchCard.initialModel)
  # on CT._candlestick (const $ BuildCandlestick BuildCandlestick.initialModel)
  # on CT._parallel (const $ BuildParallel BuildParallel.initialModel)
  # on CT._dropdown (const $ SetupDropdown SetupDropdown.initialModel)
  # on CT._radio (const $ SetupRadio SetupRadio.initialModel)
  # on CT._checkbox (const $ SetupCheckbox SetupCheckbox.initialModel)
  # on CT._text (const $ SetupText SetupText.initialModel)
  # on CT._numeric (const $ SetupNumeric SetupNumeric.initialModel)
  # on CT._time (const $ SetupTime SetupTime.initialModel)
  # on CT._date (const $ SetupDate SetupDate.initialModel)
  # on CT._datetime (const $ SetupDatetime SetupDatetime.initialModel)
  # on CT._static (const $ SetupStatic SetupStatic.initialModel)
  # on CT._markdown (const $ Markdown MD.emptyModel)
  # on CT._table (const $ Table JT.emptyModel)
  # on CT._download (const Download)
  # on CT._variables (const $ Variables Variables.emptyModel)
  # on CT._troubleshoot (const Troubleshoot)
  # on CT._cache (const $ Cache Nothing)
  # on CT._open (const $ Open Nothing)
  # on CT._downloadOptions
      (const $ DownloadOptions $ DLO.initialState
       { targetName = runFileName ∘ fileName <$> Port.extractFilePath varMap })
  # on CT._draftboard (const $ Draftboard DB.emptyModel)
  # on CT._tabs (const $ Tabs Tabs.initialModel)
  # on CT._structureEditor (const $ StructureEditor StructureEditor.initialModel)
  # on CT._geoMarker (const $ SetupGeoMarker SetupGeoMarker.initialModel)
  # on CT._geoHeatmap (const $ SetupGeoHeatmap SetupGeoHeatmap.initialModel)
  # on CT._viz (const $ Viz Viz.initial)


singletonDraftboard ∷ DeckId → AnyCardModel
singletonDraftboard deckId =
  Draftboard { layout: Pane.Cell (Just deckId) }

singletonTabs ∷ DeckId → AnyCardModel
singletonTabs deckId =
  Tabs { tabs: pure deckId }

splitDraftboard ∷ Orn.Orientation → L.List DeckId → AnyCardModel
splitDraftboard orn deckIds =
  Draftboard { layout: Pane.Split orn (mkCell <$> deckIds) }
  where
    size =
      L.length deckIds

    mkCell deckId =
      (1 % size) × Pane.wrap (Orn.reverse orn) (Pane.Cell (Just deckId))

mirrorInParent ∷ DeckId → DeckId → AnyCardModel → Maybe AnyCardModel
mirrorInParent oldId newId = case _ of
  Draftboard { layout } → do
    cursor ← Pane.getCursorFor (Just oldId) layout
    let
      cursor' = fromMaybe L.Nil (L.tail cursor)
      orn = case Pane.getAt cursor' layout of
        Just (Pane.Split o _) → o
        _ → Orn.Vertical
    layout' ←
      Layout.insertSplit
        (Pane.Cell (Just newId)) orn (1 % 2) Layout.SideB cursor' layout
    pure (Draftboard { layout: layout' })
  Tabs { tabs } →
    let
      tabs' = do
        deckId ← tabs
        if deckId ≡ oldId
          then [ deckId, newId ]
          else [ deckId ]
    in
      pure (Tabs { tabs: tabs' })
  _ → Nothing

childDeckIds ∷ AnyCardModel → L.List DeckId
childDeckIds = case _ of
  Draftboard { layout } → L.catMaybes (L.fromFoldable layout)
  Tabs { tabs } → L.fromFoldable tabs
  s → mempty

updatePointer
  ∷ DeckId
  → Maybe DeckId
  → AnyCardModel
  → AnyCardModel
updatePointer old new = case _ of
  Draftboard { layout } →
    Draftboard { layout: update <$> layout}
  Tabs { tabs } →
    Tabs { tabs: Array.mapMaybe (update ∘ Just) tabs }
  card → card

  where
    update (Just deckId) | deckId ≡ old = new
    update a = a

_SetupLabeledInput ∷ Traversal' AnyCardModel SetupLabeled.Model
_SetupLabeledInput = wander \f s → case s of
  SetupDropdown m → map SetupDropdown $ f m
  SetupRadio m → map SetupRadio $ f m
  SetupCheckbox m → map SetupCheckbox $ f m
  _ → pure s

_SetupTextLikeInput ∷ Traversal' AnyCardModel SetupTextLike.Model
_SetupTextLikeInput = wander \f s → case s of
  SetupText m → map SetupText $ f m
  SetupNumeric m → map SetupNumeric $ f m
  SetupDate m → map SetupDate $ f m
  SetupTime m → map SetupTime $ f m
  SetupDatetime m → map SetupDatetime $ f m
  _ → pure s

_BuildMetric ∷ Prism' AnyCardModel BuildMetric.Model
_BuildMetric = prism' BuildMetric case _ of
  BuildMetric a → Just a
  _ → Nothing

_BuildSankey ∷ Prism' AnyCardModel BuildSankey.Model
_BuildSankey = prism' BuildSankey case _ of
  BuildSankey a → Just a
  _ → Nothing

_BuildGauge ∷ Prism' AnyCardModel BuildGauge.Model
_BuildGauge = prism' BuildGauge case _ of
  BuildGauge a → Just a
  _ → Nothing

_BuildGraph ∷ Prism' AnyCardModel BuildGraph.Model
_BuildGraph = prism' BuildGraph case _ of
  BuildGraph a → Just a
  _ → Nothing

_BuildPie ∷ Prism' AnyCardModel BuildPie.Model
_BuildPie = prism' BuildPie case _ of
  BuildPie a → Just a
  _ → Nothing

_BuildRadar ∷ Prism' AnyCardModel BuildRadar.Model
_BuildRadar = prism' BuildRadar case _ of
  BuildRadar a → Just a
  _ → Nothing

_BuildBar ∷ Prism' AnyCardModel BuildBar.Model
_BuildBar = prism' BuildBar case _ of
  BuildBar a → Just a
  _ → Nothing

_BuildLine ∷ Prism' AnyCardModel BuildLine.Model
_BuildLine = prism' BuildLine case _ of
  BuildLine a → Just a
  _ → Nothing

_BuildArea ∷ Prism' AnyCardModel BuildArea.Model
_BuildArea = prism' BuildArea case _ of
  BuildArea a → Just a
  _ → Nothing

_BuildScatter ∷ Prism' AnyCardModel BuildScatter.Model
_BuildScatter = prism' BuildScatter case _ of
  BuildScatter a → Just a
  _ → Nothing

_BuildFunnel ∷ Prism' AnyCardModel BuildFunnel.Model
_BuildFunnel = prism' BuildFunnel case _ of
  BuildFunnel a → Just a
  _ → Nothing

_BuildCandlestick ∷ Prism' AnyCardModel BuildCandlestick.Model
_BuildCandlestick = prism' BuildCandlestick case _ of
  BuildCandlestick a → Just a
  _ → Nothing

_BuildBoxplot ∷ Prism' AnyCardModel BuildBoxplot.Model
_BuildBoxplot = prism' BuildBoxplot case _ of
  BuildBoxplot a → Just a
  _ → Nothing

_BuildHeatmap ∷ Prism' AnyCardModel BuildHeatmap.Model
_BuildHeatmap = prism' BuildHeatmap case _ of
  BuildHeatmap a → Just a
  _ → Nothing

_BuildPunchCard ∷ Prism' AnyCardModel BuildPunchCard.Model
_BuildPunchCard = prism' BuildPunchCard case _ of
  BuildPunchCard a → Just a
  _ → Nothing

_BuildParallel ∷ Prism' AnyCardModel BuildParallel.Model
_BuildParallel = prism' BuildParallel case _ of
  BuildParallel a → Just a
  _ → Nothing

_SetupGeoMarker ∷ Prism' AnyCardModel SetupGeoMarker.Model
_SetupGeoMarker = prism' SetupGeoMarker case _ of
  SetupGeoMarker a → Just a
  _ → Nothing

_SetupGeoHeatmap ∷ Prism' AnyCardModel SetupGeoHeatmap.Model
_SetupGeoHeatmap = prism' SetupGeoHeatmap case _ of
  SetupGeoHeatmap a → Just a
  _ → Nothing

setupSelect ∷ CT.Select () → SetupLabeled.Model → AnyCardModel
setupSelect fit m = case_
  # on CT._dropdown (const $ SetupDropdown m)
  # on CT._radio (const $ SetupRadio m)
  # on CT._checkbox (const $ SetupCheckbox m)
  $ fit

setupInput ∷ CT.Input () → SetupTextLike.Model → AnyCardModel
setupInput fit m = case_
  # on CT._text (const $ SetupText m)
  # on CT._numeric (const $ SetupNumeric m)
  # on CT._date (const $ SetupDate m)
  # on CT._time (const $ SetupTime m)
  # on CT._datetime (const $ SetupDatetime m)
  $ fit
