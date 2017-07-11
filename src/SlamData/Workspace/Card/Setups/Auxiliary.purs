module SlamData.Workspace.Card.Setups.Auxiliary where

import SlamData.Prelude

import Control.Alternative (class Alternative)

import Data.Argonaut as J
import Data.Variant as V
import Data.Functor.Variant (VariantF, FProxy)
import Data.Newtype (under)
import Data.Profunctor (dimap)

import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.Profunctor as HPR
import Halogen.Component.Proxy as HCP

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Setups.Auxiliary.Area as Area
import SlamData.Workspace.Card.Setups.Auxiliary.Bar as Bar
import SlamData.Workspace.Card.Setups.Auxiliary.Funnel as Funnel
import SlamData.Workspace.Card.Setups.Auxiliary.Graph as Graph
import SlamData.Workspace.Card.Setups.Auxiliary.Heatmap as Heatmap
import SlamData.Workspace.Card.Setups.Auxiliary.Line as Line
import SlamData.Workspace.Card.Setups.Auxiliary.Metric as Metric
import SlamData.Workspace.Card.Setups.Auxiliary.PunchCard as PunchCard
import SlamData.Workspace.Card.Setups.Auxiliary.Scatter as Scatter
import SlamData.Workspace.Card.Setups.Auxiliary.GeoHeatmap as GeoHeatmap
import SlamData.Workspace.Card.Setups.Auxiliary.GeoMarker as GeoMarker

import Test.StrongCheck.Gen as Gen

type State = Variant
  ( area ∷ Area.State
  , bar ∷ Bar.State
  , funnel ∷ Funnel.State
  , graph ∷ Graph.State
  , heatmap ∷ Heatmap.State
  , line ∷ Line.State
  , metric ∷ Metric.State
  , punchCard ∷ PunchCard.State
  , scatter ∷ Scatter.State
  , geoHeatmap ∷ GeoHeatmap.State
  , geoMarker ∷ GeoMarker.State
  )

eq_ ∷ State → State → Boolean
eq_ r = V.default false
  # V.on CT._area (\rr → V.on CT._area (Area.eq_ rr) ff r)
  # V.on CT._bar (\rr → V.on CT._bar (Bar.eq_ rr) ff r)
  # V.on CT._funnel (\rr → V.on CT._funnel (Funnel.eq_ rr) ff r)
  # V.on CT._graph (\rr → V.on CT._graph (Graph.eq_ rr) ff r)
  # V.on CT._heatmap (\rr → V.on CT._heatmap (Heatmap.eq_ rr) ff r)
  # V.on CT._line (\rr → V.on CT._line (Line.eq_ rr) ff r)
  # V.on CT._metric (\rr → V.on CT._metric (Metric.eq_ rr) ff r)
  # V.on CT._punchCard (\rr → V.on CT._punchCard (PunchCard.eq_ rr) ff r)
  # V.on CT._scatter (\rr → V.on CT._scatter (Scatter.eq_ rr) ff r)
  # V.on CT._geoHeatmap (\rr → V.on CT._geoHeatmap (GeoHeatmap.eq_ rr) ff r)
  # V.on CT._geoMarker (\rr → V.on CT._geoMarker (GeoMarker.eq_ rr) ff r)

encode ∷ State → J.Json
encode = V.case_
  # V.on CT._area Area.encode
  # V.on CT._bar Bar.encode
  # V.on CT._funnel Funnel.encode
  # V.on CT._graph Graph.encode
  # V.on CT._heatmap Heatmap.encode
  # V.on CT._line Line.encode
  # V.on CT._metric Metric.encode
  # V.on CT._punchCard PunchCard.encode
  # V.on CT._scatter Scatter.encode
  # V.on CT._geoHeatmap GeoHeatmap.encode
  # V.on CT._geoMarker GeoMarker.encode

decode ∷ J.Json → String ⊹ State
decode j =
  (map (V.inj CT._area) $ Area.decode j)
  <|> (map (V.inj CT._bar) $ Bar.decode j)
  <|> (map (V.inj CT._funnel) $ Funnel.decode j)
  <|> (map (V.inj CT._graph) $ Graph.decode j)
  <|> (map (V.inj CT._heatmap) $ Heatmap.decode j)
  <|> (map (V.inj CT._line) $ Line.decode j)
  <|> (map (V.inj CT._metric) $ Metric.decode j)
  <|> (map (V.inj CT._punchCard) $ PunchCard.decode j)
  <|> (map (V.inj CT._scatter) $ Scatter.decode j)
  <|> (map (V.inj CT._geoHeatmap) $ GeoHeatmap.decode j)
  <|> (map (V.inj CT._geoMarker) $ GeoMarker.decode j)

gen ∷ Gen.Gen State
gen = Gen.oneOf ( map (V.inj CT._area) Area.gen )
  [ map (V.inj CT._bar) Bar.gen
  , map (V.inj CT._funnel) Funnel.gen
  , map (V.inj CT._graph) Graph.gen
  , map (V.inj CT._heatmap) Heatmap.gen
  , map (V.inj CT._line) Line.gen
  , map (V.inj CT._metric) Metric.gen
  , map (V.inj CT._punchCard) PunchCard.gen
  , map (V.inj CT._scatter) Scatter.gen
  , map (V.inj CT._geoHeatmap) GeoHeatmap.gen
  , map (V.inj CT._geoMarker) GeoMarker.gen
  ]

type Query = VariantF
  ( area ∷ FProxy Area.Query
  , bar ∷ FProxy Bar.Query
  , funnel ∷ FProxy Funnel.Query
  , graph ∷ FProxy Graph.Query
  , heatmap ∷ FProxy Heatmap.Query
  , line ∷ FProxy Line.Query
  , metric ∷ FProxy Metric.Query
  , punchCard ∷ FProxy PunchCard.Query
  , scatter ∷ FProxy Scatter.Query
  , geoHeatmap ∷ FProxy GeoHeatmap.Query
  , geoMarker ∷ FProxy GeoMarker.Query
  )

injAux
  ∷ ∀ q st m v1 v2 s
  . RowCons s st v1 v2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → H.Component HH.HTML q (Maybe st) st m
  → H.Component HH.HTML q (Variant v2) (Variant v2) m
injAux proxy =
  under HPR.ProComponent
  $ dimap (V.prj proxy) (V.inj proxy)

type UnifiedAux m =
  H.Component HH.HTML (HCP.ProxyQ (Const Void) State State) State State m

area ∷ ∀ m. UnifiedAux m
area = HCP.proxy $ injAux CT._area Area.component

bar ∷ ∀ m. UnifiedAux m
bar = HCP.proxy $ injAux CT._bar Bar.component

funnel ∷ ∀ m. UnifiedAux m
funnel = HCP.proxy $ injAux CT._funnel Funnel.component

graph ∷ ∀ m. UnifiedAux m
graph = HCP.proxy $ injAux CT._graph Graph.component

heatmap ∷ ∀ m. UnifiedAux m
heatmap = HCP.proxy $ injAux CT._heatmap Heatmap.component

line ∷ ∀ m. UnifiedAux m
line = HCP.proxy $ injAux CT._line Line.component

metric ∷ ∀ m. UnifiedAux m
metric = HCP.proxy $ injAux CT._metric Metric.component

punchCard ∷ ∀ m. UnifiedAux m
punchCard = HCP.proxy $ injAux CT._punchCard PunchCard.component

scatter ∷ ∀ m. UnifiedAux m
scatter = HCP.proxy $ injAux CT._scatter Scatter.component

geoHeatmap ∷ ∀ m. UnifiedAux m
geoHeatmap = HCP.proxy $ injAux CT._geoHeatmap GeoHeatmap.component

geoMarker ∷ ∀ m. UnifiedAux m
geoMarker = HCP.proxy $ injAux CT._geoMarker GeoMarker.component

cardTypeAux ∷ ∀ m f. Alternative f ⇒ CT.CardType → f (UnifiedAux m)
cardTypeAux = V.default empty
  # V.on CT._area (const $ pure area)
  # V.on CT._bar (const $ pure bar)
  # V.on CT._funnel (const $ pure funnel)
  # V.on CT._graph (const $ pure graph)
  # V.on CT._heatmap (const $ pure heatmap)
  # V.on CT._line (const $ pure line)
  # V.on CT._metric (const $ pure metric)
  # V.on CT._punchCard (const $ pure punchCard)
  # V.on CT._scatter (const $ pure scatter)
  # V.on CT._geoHeatmap (const $ pure geoHeatmap)
  # V.on CT._geoMarker (const $ pure geoMarker)
