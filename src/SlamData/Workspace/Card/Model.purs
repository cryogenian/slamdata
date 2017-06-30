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
import Data.Array as Array
import Data.List as L
import Data.Path.Pathy (fileName, runFileName)
import Data.Rational ((%))
import Data.StrMap as StrMap

import SlamData.Workspace.Card.Ace.Model as Ace
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Chart.Model as Chart
import SlamData.Workspace.Card.DownloadOptions.Component.State as DLO
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Model as DB
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.FormInput.Model as FormInput
import SlamData.Workspace.Card.Geo.Model as Geo
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Open.Model as Open
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Query.Model as Query
import SlamData.Workspace.Card.Setups.Viz.Model as SetupViz
import SlamData.Workspace.Card.StructureEditor.Model as StructureEditor
import SlamData.Workspace.Card.Table.Model as JT
import SlamData.Workspace.Card.Tabs.Model as Tabs
import SlamData.Workspace.Card.Variables.Model as Variables
import SlamData.Workspace.Card.Viz.Model as Viz
import SlamData.Workspace.Card.Metric.Model as Metric
import SlamData.Workspace.Card.PivotTable.Model as PivotTable
import SlamData.Workspace.Deck.DeckId (DeckId)
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data AnyCardModel
  = Ace CT.AceMode Ace.Model
  | Search String
  | Chart Chart.Model
  | Markdown MD.Model
  | Table JT.Model
  | Download
  | Variables Variables.Model
  | Troubleshoot
  | Cache (Maybe String)
  | Open Open.Model
  | DownloadOptions DLO.State
  | Draftboard DB.Model
  | FormInput FormInput.Model
  | Tabs Tabs.Model
  | StructureEditor StructureEditor.Model
  | Geo Geo.Model
  | SetupViz SetupViz.Model
  | Viz Viz.Model
  | Metric Metric.Model
  | PivotTable PivotTable.Model

instance arbitraryAnyCardModel ∷ SC.Arbitrary AnyCardModel where
  arbitrary =
    Gen.oneOf (pure Download)
      [ Ace <$> SC.arbitrary <*> Ace.genModel
      , Search <$> SC.arbitrary
      , Chart <$> Chart.genModel
      , Markdown <$> MD.genModel
      , Table <$> JT.genModel
      , Variables <$> Variables.genModel
      , pure Troubleshoot
      , Cache <$> SC.arbitrary
      , Open <$> SC.arbitrary
      , Draftboard <$> DB.genModel
      , FormInput <$> FormInput.genModel
      , Tabs <$> Tabs.genModel
      , StructureEditor <$> StructureEditor.genModel
      , Geo <$> Geo.genModel
      , SetupViz <$> SetupViz.genModel
      , Viz <$> Viz.genModel
      , Metric <$> Metric.genModel
      , PivotTable <$> PivotTable.genModel
      ]

updateCardModel ∷ AnyCardModel → AnyCardModel → AnyCardModel
updateCardModel = case _, _ of
  Markdown author, Markdown consumer →
    Markdown $ StrMap.union author consumer
  Search _, Search consumer →
    Search consumer
  FormInput _, FormInput consumer ->
    FormInput consumer
  author, _ →
    author

instance eqAnyCardModel ∷ Eq AnyCardModel where
  eq = case _, _ of
    Ace x1 y1, Ace x2 y2 → x1 ≡ x2 && Ace.eqModel y1 y2
    Search s1, Search s2 → s1 ≡ s2
    Chart x, Chart y → Chart.eqModel x y
    Markdown x, Markdown y → x ≡ y
    Table x, Table y → JT.eqModel x y
    Download, Download → true
    Variables x, Variables y → Variables.eqModel x y
    Troubleshoot, Troubleshoot → true
    Cache x, Cache y → x ≡ y
    Open x, Open y → x ≡ y
    DownloadOptions x, DownloadOptions y → DLO.eqState x y
    Draftboard x, Draftboard y → DB.eqModel x y
    FormInput x, FormInput y → FormInput.eqModel x y
    Tabs x, Tabs y → Tabs.eqModel x y
    StructureEditor x, StructureEditor y → x == y
    Geo x, Geo y → Geo.eqModel x y
    SetupViz x, SetupViz y → SetupViz.eqModel x y
    Viz x, Viz y → Viz.eqModel x y
    Metric x, Metric y → Metric.eqModel x y
    PivotTable x, PivotTable y → PivotTable.eqModel x y
    _, _ → false

instance encodeJsonCardModel ∷ J.EncodeJson AnyCardModel where
  encodeJson = encode

instance decodeJsonCardModel ∷ J.DecodeJson AnyCardModel where
  decodeJson = decode

modelCardType ∷ AnyCardModel → CT.CardType
modelCardType = case _ of
  Ace mode _ → CT.Ace mode
  Search _ → CT.Search
  Chart _ → CT.Chart
  Geo _ → CT.GeoChart
  Markdown _ → CT.Markdown
  Table _ → CT.Table
  Download → CT.Download
  Variables _ → CT.Variables
  Troubleshoot → CT.Troubleshoot
  Cache _ → CT.Cache
  Open _ → CT.Open
  DownloadOptions _ → CT.DownloadOptions
  Draftboard _ → CT.Draftboard
  FormInput _ → CT.FormInput
  Tabs _ → CT.Tabs
  StructureEditor _ → CT.StructureEditor
  SetupViz _ → CT.SetupViz
  Viz _ → CT.Viz
  Metric _ → CT.Metric
  PivotTable _ → CT.PivotTable

encode ∷ AnyCardModel → J.Json
encode card =
  "cardType" := modelCardType card
    ~> "model" := encodeCardModel card
    ~> J.jsonEmptyObject

decode ∷ J.Json → Either String AnyCardModel
decode js = do
  obj ← J.decodeJson js
  cardType ← obj .? "cardType"
  model ← obj .? "model"
  decodeCardModel cardType model

encodeCardModel ∷ AnyCardModel → J.Json
encodeCardModel = case _ of
  Ace mode model → Ace.encode model
  Search txt → J.encodeJson txt
  Chart model → Chart.encode model
  Geo model → Geo.encode model
  Markdown model → MD.encode model
  Table model → JT.encode model
  Download → J.jsonEmptyObject
  Variables model → Variables.encode model
  Troubleshoot → J.jsonEmptyObject
  Cache model → J.encodeJson model
  Open res → Open.encode res
  DownloadOptions model → DLO.encode model
  Draftboard model → DB.encode model
  FormInput model → FormInput.encode model
  Tabs model → Tabs.encode model
  StructureEditor model → StructureEditor.encode model
  SetupViz model → SetupViz.encode model
  Viz model → Viz.encode model
  Metric model → Metric.encode model
  PivotTable model → PivotTable.encode model

decodeCardModel
  ∷ CT.CardType
  → J.Json
  → Either String AnyCardModel
decodeCardModel = case _ of
  CT.Ace mode → map (Ace mode) ∘ Ace.decode
  CT.Search → map Search ∘ J.decodeJson
  CT.FormInput → map FormInput ∘ FormInput.decode
  CT.Chart → map Chart ∘ Chart.decode
  CT.Markdown → map Markdown ∘ MD.decode
  CT.Table → map Table ∘ JT.decode
  CT.Download → const $ pure Download
  CT.Variables → map Variables ∘ Variables.decode
  CT.Troubleshoot → const $ pure Troubleshoot
  CT.Cache → map Cache ∘ J.decodeJson
  CT.Open → map Open ∘ decodeOpen
  CT.DownloadOptions → map DownloadOptions ∘ DLO.decode
  CT.Draftboard → map Draftboard ∘ DB.decode
  CT.Tabs → map Tabs ∘ Tabs.decode
  CT.StructureEditor → map StructureEditor ∘ StructureEditor.decode
  CT.GeoChart → map Geo ∘ Geo.decode
  CT.SetupViz → map SetupViz ∘ SetupViz.decode
  CT.Viz → map Viz ∘ Viz.decode
  CT.Metric → map Metric ∘ Metric.decode
  CT.PivotTable → map PivotTable ∘ PivotTable.decode
  where
    -- For backwards compat
    decodeOpen j =
      Open.decode j <|> (map Open.Resource <$> J.decodeJson j)

cardModelOfType ∷ Port.Out → CT.CardType → AnyCardModel
cardModelOfType (port × varMap) = case _ of
  CT.Ace CT.SQLMode → Ace CT.SQLMode (Query.initialModel port)
  CT.Ace mode → Ace mode Ace.emptyModel
  CT.Search → Search ""
  CT.FormInput → FormInput FormInput.initialModel
  CT.Chart → Chart Chart.emptyModel
  CT.Markdown → Markdown MD.emptyModel
  CT.Table → Table JT.emptyModel
  CT.Download → Download
  CT.Variables → Variables Variables.emptyModel
  CT.Troubleshoot → Troubleshoot
  CT.Cache → Cache Nothing
  CT.Open → Open Nothing
  CT.DownloadOptions →
    DownloadOptions
    $ DLO.initialState
    { targetName = runFileName ∘ fileName <$> Port.extractFilePath varMap }
  CT.Draftboard → Draftboard DB.emptyModel
  CT.Tabs → Tabs Tabs.initialModel
  CT.StructureEditor → StructureEditor StructureEditor.initialModel
  CT.GeoChart → Geo Geo.initialModel
  CT.SetupViz → SetupViz SetupViz.initialModel
  CT.Viz → Viz Viz.initialModel
  CT.Metric → Metric Metric.initialModel
  CT.PivotTable → PivotTable PivotTable.initialModel

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
