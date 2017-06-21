module SlamData.Workspace.Card.Setups.Viz.Model where

import SlamData.Prelude

import Data.Array as A
import Data.Path.Pathy ((</>), (<.>), file, rootDir, dir)
import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Map as Map
import Data.Newtype (un)
import Data.StrMap as SM
import Data.Foldable as F
import Data.URI (URIRef)
import Data.URI as URI
import Data.Variant (inj, case_, on, default)

import Global (encodeURIComponent, decodeURIComponent)

import SlamData.Common.Align (Align(..))
import SlamData.Common.Sort (Sort(..))
import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (ColorScheme(..))
import SlamData.Workspace.Card.Geo.Model (onURIRef)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type GeoHeatmap =
  { osmURI ∷ URIRef }

formatter ∷ String
formatter = ""

osmURI ∷ URIRef
osmURI =
  Left $ URI.URI
  (Just $ URI.URIScheme "http")
  (URI.HierarchicalPart
   (Just $ URI.Authority Nothing [(URI.NameAddress "{s}.tile.osm.org") × Nothing])
   (Just $ Right $ rootDir </> dir "{z}" </> dir "{x}" </> file "{y}" <.> "png"))
  Nothing
  Nothing

minSize ∷ Number
minSize = 10.0

maxSize ∷ Number
maxSize = 50.0

isSmooth ∷ Boolean
isSmooth = false

isStacked ∷ Boolean
isStacked = false

size ∷ Number
size = 10.0

axisLabelAngle ∷ Number
axisLabelAngle = 0.0

circular ∷ Boolean
circular = false

isColorSchemeReversed ∷ Boolean
isColorSchemeReversed = false

colorScheme ∷ ColorScheme
colorScheme = RedToBlue

order ∷ Sort
order = Asc

align ∷ Align
align = LeftAlign

minValue ∷ Number
minValue = 1.0

maxValue ∷ Number
maxValue = 50.0

optionalMarkers ∷ Boolean
optionalMarkers = false

initialGeoHeatmap ∷ GeoHeatmap
initialGeoHeatmap =
  { osmURI }

eqGeoHeatmap ∷ GeoHeatmap → GeoHeatmap → Boolean
eqGeoHeatmap r1 r2 =
  r1.osmURI ≡ r2.osmURI

encodeGeoHeatmap ∷ GeoHeatmap → J.Json
encodeGeoHeatmap r =
  "aux" := "geo-heatmap"
  ~> "osmURI" := (URI.printURIRef $ onURIRef encodeURIComponent r.osmURI)
  ~> J.jsonEmptyObject

decodeGeoHeatmap ∷ J.JObject → String ⊹ GeoHeatmap
decodeGeoHeatmap obj = do
  osmURIStr ← obj .? "osmURI"
  osmURI ←
    map (onURIRef decodeURIComponent)
    $ lmap (\x → show x <> ":" <> osmURIStr)
    $ URI.runParseURIRef osmURIStr
  pure { osmURI }

type GeoMarker =
  { minSize ∷ Number
  , maxSize ∷ Number
  , osmURI ∷ URIRef
  }

initialGeoMarker ∷ GeoMarker
initialGeoMarker =
  { osmURI
  , minSize
  , maxSize
  }

eqGeoMarker ∷ GeoMarker → GeoMarker → Boolean
eqGeoMarker r1 r2 =
  r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize
  ∧ r1.osmURI ≡ r2.osmURI

encodeGeoMarker ∷ GeoMarker → J.Json
encodeGeoMarker r =
  "aux" := "geo-marker"
  ~> "osmURI" := (URI.printURIRef $ onURIRef encodeURIComponent r.osmURI)
  ~> "maxSize" := r.maxSize
  ~> "minSize" := r.minSize
  ~> J.jsonEmptyObject

decodeGeoMarker ∷ J.JObject → String ⊹ GeoMarker
decodeGeoMarker obj = do
  osmURIStr ← obj .? "osmURI"
  osmURI ←
    map (onURIRef decodeURIComponent)
    $ lmap (\x → show x <> ":" <> osmURIStr)
    $ URI.runParseURIRef osmURIStr
  minSize ← obj .? "minSize"
  maxSize ← obj .? "maxSize"
  pure $ { minSize, maxSize, osmURI }

type Area =
  { isStacked ∷ Boolean
  , isSmooth ∷ Boolean
  , axisLabelAngle ∷ Number
  , size ∷ Number
  }

initialArea ∷ Area
initialArea =
  { isStacked
  , isSmooth
  , axisLabelAngle
  , size
  }

eqArea ∷ Area → Area → Boolean
eqArea r1 r2 =
  r1.isStacked ≡ r2.isStacked
  ∧ r1.isSmooth ≡ r2.isSmooth
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle
  ∧ r1.size ≡ r2.size

encodeArea ∷ Area → J.Json
encodeArea r =
  "aux" := "area"
  ~> "isStacked" := r.isStacked
  ~> "isSmooth" := r.isSmooth
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> "size" := r.size
  ~> J.jsonEmptyObject

decodeArea ∷ J.JObject → String ⊹ Area
decodeArea obj = do
  isStacked ← obj .? "isStacked"
  isSmooth ← obj .? "isSmooth"
  axisLabelAngle ← obj .? "axisLabelAngle"
  size ← obj .? "size"
  pure { isStacked
       , isSmooth
       , axisLabelAngle
       , size
       }
type Bar =
  { axisLabelAngle ∷ Number }


initialBar ∷ Bar
initialBar =
  { axisLabelAngle }

eqBar ∷ Bar → Bar → Boolean
eqBar r1 r2 =
  r1.axisLabelAngle ≡ r2.axisLabelAngle

encodeBar ∷ Bar → J.Json
encodeBar r =
  "aux" := "bar"
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> J.jsonEmptyObject

decodeBar ∷ J.JObject → String ⊹ Bar
decodeBar obj = do
  axisLabelAngle ← obj .? "axisLabelAngle"
  pure { axisLabelAngle }

type Funnel =
  { align ∷ Align
  , order ∷ Sort
  }

initialFunnel ∷ Funnel
initialFunnel =
  { order
  , align
  }

eqFunnel ∷ Funnel → Funnel → Boolean
eqFunnel r1 r2 =
  r1.align ≡ r2.align
  ∧ r1.order ≡ r2.order

encodeFunnel ∷ Funnel → J.Json
encodeFunnel r =
  "aux" := "funnel"
  ~> "order" := r.order
  ~> "align" := r.align
  ~> J.jsonEmptyObject

decodeFunnel ∷ J.JObject → String ⊹ Funnel
decodeFunnel obj = do
  order ← obj .? "order"
  align ← obj .? "align"
  pure { order, align }

type Graph =
  { circular ∷ Boolean
  , maxSize ∷ Number
  , minSize ∷ Number
  }

initialGraph ∷ Graph
initialGraph =
  { minSize
  , maxSize
  , circular
  }

eqGraph ∷ Graph → Graph → Boolean
eqGraph r1 r2 =
  r1.circular ≡ r2.circular
  ∧ r1.maxSize ≡ r2.maxSize
  ∧ r1.minSize ≡ r2.minSize

encodeGraph ∷ Graph → J.Json
encodeGraph r =
  "aux" := "graph"
  ~> "circular" := r.circular
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> J.jsonEmptyObject

decodeGraph ∷ J.JObject → String ⊹ Graph
decodeGraph obj = do
  circular ← obj .? "circular"
  minSize ← obj .? "minSize"
  maxSize ← obj .? "maxSize"
  pure { circular, minSize, maxSize }

type Heatmap =
  { colorScheme ∷ ColorScheme
  , isColorSchemeReversed ∷ Boolean
  , minValue ∷ Number
  , maxValue ∷ Number
  }

initialHeatmap ∷ Heatmap
initialHeatmap =
  { minValue
  , maxValue
  , isColorSchemeReversed
  , colorScheme
  }

eqHeatmap ∷ Heatmap → Heatmap → Boolean
eqHeatmap r1 r2 =
  r1.colorScheme ≡ r2.colorScheme
  ∧ r1.isColorSchemeReversed ≡ r2.isColorSchemeReversed
  ∧ r1.minValue ≡ r2.minValue
  ∧ r1.maxValue ≡ r2.maxValue

encodeHeatmap ∷ Heatmap → J.Json
encodeHeatmap r =
  "aux" := "heatmap"
  ~> "colorScheme" := r.colorScheme
  ~> "isColorSchemeReversed" := r.isColorSchemeReversed
  ~> "minValue" := r.minValue
  ~> "maxValue" := r.maxValue
  ~> J.jsonEmptyObject

decodeHeatmap ∷ J.JObject → String ⊹ Heatmap
decodeHeatmap obj = do
  colorScheme ← obj .? "colorScheme"
  isColorSchemeReversed ← obj .? "isColorSchemeReversed"
  minValue ← obj .? "minValue"
  maxValue ← obj .? "maxValue"
  pure { colorScheme
       , isColorSchemeReversed
       , minValue
       , maxValue
       }

type Line =
  { maxSize ∷ Number
  , minSize ∷ Number
  , axisLabelAngle ∷ Number
  , optionalMarkers ∷ Boolean
  }

initialLine ∷ Line
initialLine =
  { maxSize
  , minSize
  , axisLabelAngle
  , optionalMarkers
  }

eqLine ∷ Line → Line → Boolean
eqLine r1 r2 =
  r1.maxSize ≡ r2.maxSize
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle
  ∧ r1.optionalMarkers ≡ r2.optionalMarkers

encodeLine ∷ Line → J.Json
encodeLine r =
  "aux" := "line"
  ~> "maxSize" := r.maxSize
  ~> "minSize" := r.minSize
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> "optionalMarkers" := r.optionalMarkers
  ~> J.jsonEmptyObject

decodeLine ∷ J.JObject → String ⊹ Line
decodeLine obj = do
  maxSize ← obj .? "maxSize"
  minSize ← obj .? "minSize"
  axisLabelAngle ← obj .? "axisLabelAngle"
  optionalMarkers ← obj .? "optionalMarkers"
  pure { maxSize
        , minSize
        , axisLabelAngle
        , optionalMarkers
        }

type Metric =
  { formatter ∷ String }

initialMetric ∷ Metric
initialMetric =
  { formatter }

eqMetric ∷ Metric → Metric → Boolean
eqMetric r1 r2 =
  r1.formatter ≡ r2.formatter

encodeMetric ∷ Metric → J.Json
encodeMetric r =
  "aux" := "metric"
  ~> "formatter" := r.formatter
  ~> J.jsonEmptyObject

decodeMetric ∷ J.JObject → String ⊹ Metric
decodeMetric obj = do
  formatter ← obj .? "formatter"
  pure { formatter }

type PunchCard =
  { minSize ∷ Number
  , maxSize ∷ Number
  }

initialPunchCard ∷ PunchCard
initialPunchCard =
  { minSize
  , maxSize
  }

eqPunchCard ∷ PunchCard → PunchCard → Boolean
eqPunchCard r1 r2 =
  r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize

encodePunchCard ∷ PunchCard → J.Json
encodePunchCard r =
  "aux" := "punch-card"
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> J.jsonEmptyObject

decodePunchCard ∷ J.JObject → String ⊹ PunchCard
decodePunchCard obj = do
  minSize ← obj .? "minSize"
  maxSize ← obj .? "maxSize"
  pure { minSize, maxSize }

type Scatter =
  { minSize ∷ Number
  , maxSize ∷ Number
  }

initialScatter ∷ Scatter
initialScatter =
  { minSize
  , maxSize
  }

eqScatter ∷ Scatter → Scatter → Boolean
eqScatter r1 r2 =
  r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize

encodeScatter ∷ Scatter → J.Json
encodeScatter r =
  "aux" := "scatter"
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> J.jsonEmptyObject

decodeScatter ∷ J.JObject → String ⊹ Scatter
decodeScatter obj = do
  minSize ← obj .? "minSize"
  maxSize ← obj .? "maxSize"
  pure { minSize, maxSize }

encodeOther ∷ Unit → J.Json
encodeOther _ =
  "aux" := "other"
  ~> J.jsonEmptyObject

decodeOther ∷ J.JObject → String ⊹ Unit
decodeOther _ = do
  pure unit

type Aux = Variant
  ( geoHeatmap ∷ GeoHeatmap
  , geoMarker ∷ GeoMarker
  , area ∷ Area
  , bar ∷ Bar
  , funnel ∷ Funnel
  , graph ∷ Graph
  , heatmap ∷ Heatmap
  , line ∷ Line
  , metric ∷ Metric
  , punchCard ∷ PunchCard
  , scatter ∷ Scatter
  , other ∷ Unit
  )

initialAuxMap ∷ Map.Map VT.VizType Aux
initialAuxMap = Map.fromFoldable
  [ VT.Geo VT.GeoHeatmap × inj _geoHeatmap initialGeoHeatmap
  , VT.Geo VT.GeoMarker × inj _geoMarker initialGeoMarker
  , VT.Chart VT.Area × inj _area initialArea
  , VT.Chart VT.Bar × inj _bar initialBar
  , VT.Chart VT.Funnel × inj _funnel initialFunnel
  , VT.Chart VT.Graph × inj _graph initialGraph
  , VT.Chart VT.Heatmap × inj _heatmap initialHeatmap
  , VT.Chart VT.Line × inj _line initialLine
  , VT.Metric × inj _metric initialMetric
  , VT.Chart VT.PunchCard × inj _punchCard initialPunchCard
  , VT.Chart VT.Scatter × inj _scatter initialScatter
  ]

_geoHeatmap = SProxy ∷ SProxy "geoHeatmap"
_geoMarker = SProxy ∷ SProxy "geoMarker"
_area = SProxy ∷ SProxy "area"
_bar = SProxy ∷ SProxy "bar"
_funnel = SProxy ∷ SProxy "funnel"
_graph = SProxy ∷ SProxy "graph"
_heatmap = SProxy ∷ SProxy "heatmap"
_line = SProxy ∷ SProxy "line"
_metric = SProxy ∷ SProxy "metric"
_punchCard = SProxy ∷ SProxy "punchCard"
_scatter = SProxy ∷ SProxy "scatter"
_other = SProxy ∷ SProxy "other"

eqAux ∷ Aux → Aux → Boolean
eqAux a = case_
  # on _geoHeatmap (\r1 → on _geoHeatmap (eqGeoHeatmap r1) (default false) a)
  # on _geoMarker (\r1 → on _geoMarker (eqGeoMarker r1) (default false) a)
  # on _area (\r1 → on _area (eqArea r1) (default false) a)
  # on _bar (\r1 → on _bar (eqBar r1) (default false) a)
  # on _funnel (\r1 → on _funnel (eqFunnel r1) (default false) a)
  # on _graph (\r1 → on _graph (eqGraph r1) (default false) a)
  # on _heatmap (\r1 → on _heatmap (eqHeatmap r1) (default false) a)
  # on _line (\r1 → on _line (eqLine r1) (default false) a)
  # on _metric (\r1 → on _metric (eqMetric r1) (default false) a)
  # on _punchCard (\r1 → on _punchCard (eqPunchCard r1) (default false) a)
  # on _scatter (\r1 → on _scatter (eqScatter r1) (default false) a)
  # on _other (const $ on _other (const true) (default false) a)

encodeAux ∷ Aux → J.Json
encodeAux = case_
  # on _geoHeatmap encodeGeoHeatmap
  # on _geoMarker encodeGeoMarker
  # on _area encodeArea
  # on _bar encodeBar
  # on _funnel encodeFunnel
  # on _graph encodeGraph
  # on _heatmap encodeHeatmap
  # on _line encodeLine
  # on _metric encodeMetric
  # on _punchCard encodePunchCard
  # on _scatter encodeScatter
  # on _other encodeOther

decodeAux ∷ J.Json → String ⊹ Aux
decodeAux = J.decodeJson >=> \obj → do
  aux ← obj .? "aux"
  case aux of
    "geoHeatmap" → inj _geoHeatmap <$> decodeGeoHeatmap obj
    "geoMarker" → inj _geoMarker <$> decodeGeoMarker obj
    "area" → inj _area <$> decodeArea obj
    "bar" → inj _bar <$> decodeBar obj
    "funnel" → inj _funnel <$> decodeFunnel obj
    "graph" → inj _graph <$> decodeGraph obj
    "heatmap" → inj _heatmap <$> decodeHeatmap obj
    "line" → inj _line <$> decodeLine obj
    "metric" → inj _metric <$> decodeMetric obj
    "punch-card" → inj _punchCard <$> decodePunchCard obj
    "scatter" → inj _scatter <$> decodeScatter obj
    other → inj _other <$> decodeOther obj


type Model =
  { dimMaps ∷ Map.Map VT.VizType T.DimensionMap
  , vizType ∷ VT.VizType
  , aux ∷ Map.Map VT.VizType Aux
  }

initialModel ∷ Model
initialModel =
  { dimMaps: Map.empty
  , vizType: VT.Chart VT.Pie
  , aux: Map.empty
  }

eqModel ∷ Model → Model → Boolean
eqModel r1 r2 =
  r1.dimMaps ≡ r2.dimMaps
  ∧ eqVTMaps r1.aux r2.aux
  ∧ r1.vizType ≡ r2.vizType
  where
  eqVTMaps vt1 vt2 =
    F.and
    $ A.zipWith (\(k1 × v1) (k2 × v2) → k1 ≡ k2 ∧ eqAux v1 v2)
      (Map.toUnfoldable r1.aux)
      (Map.toUnfoldable r2.aux)


genModel ∷ Gen.Gen Model
genModel = do
  vizType ← arbitrary
  vizTypes ← arbitrary
  pairs ← A.foldRecM foldMapFn [ ] vizTypes
  let dimMaps = Map.fromFoldable pairs
  pure { vizType, dimMaps, aux: Map.empty } --TODO: gen aux
  where
  foldMapFn
    ∷ Array (VT.VizType × T.DimensionMap)
    → VT.VizType
    → Gen.Gen (Array (VT.VizType × T.DimensionMap))
  foldMapFn arr vt = do
    dm ← genDimMap
    pure $ A.cons (vt × dm) arr

  foldFn
    ∷ Array (String × D.LabeledJCursor) → String → Gen.Gen (Array (String × D.LabeledJCursor))
  foldFn arr key = do
    val ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    pure $ A.cons (key × val) arr

  genDimMap ∷ Gen.Gen T.DimensionMap
  genDimMap = do
    keys ← arbitrary
    tpls ← A.foldRecM foldFn [ ] keys
    pure $ SM.fromFoldable tpls


encode ∷ Model → J.Json
encode r =
  "vizType" := r.vizType
  ~> "dimMaps" := r.dimMaps
  ~> "aux" := map encodeAux r.aux
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode = J.decodeJson >=> \obj → do
  vizType ← obj .? "vizType"
  dimMaps ← obj .? "dimMaps"
  aux ← traverse decodeAux =<< obj .? "aux"
  pure { vizType, dimMaps, aux }
