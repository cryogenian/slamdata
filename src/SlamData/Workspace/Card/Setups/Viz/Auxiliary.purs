module SlamData.Workspace.Card.Setups.Viz.Auxiliary where

import SlamData.Prelude

import Data.Array as Arr
import Data.URI (URIRef, runParseURIRef, printURIRef)
import Data.Symbol (class IsSymbol)
import Data.Functor.Variant (FProxy, VariantF, inj, case_, on)
import Data.Variant as V
import Data.Lens ((^.), Lens', (.~), (%~))
import Data.Lens.Record (prop)
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX
import Data.Profunctor (dimap)
import Data.Newtype (under)

import Global (decodeURIComponent, readFloat, isNaN)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Profunctor as HPR

import SlamData.Workspace.Card.Setups.Chart.ColorScheme (ColorScheme(..), colorSchemeSelect)
import SlamData.Form.Select (class OptionVal, Select(..), stringVal)
import SlamData.Common.Sort (sortSelect, Sort(..))
import SlamData.Common.Align (alignSelect, Align(..))
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Setups.Viz.Model as M
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Geo.Model (onURIRef)
import SlamData.Render.ClassName as CN

class IsSymbol s ⇐ HasLabel s where
  label ∷ SProxy s → String

instance isSmoothHasLabel ∷ HasLabel "isSmooth" where
  label _ = "Smooth"
instance isStackedHasLabel ∷ HasLabel "isStacked" where
  label _ = "Stacked"
instance sizeHasLabel ∷ HasLabel "size" where
  label _ = "Size"
instance valHasLabel ∷ HasLabel "val" where
  label _ = "Value"
instance axisLabelAngleHasLabel ∷ HasLabel "axisLabelAngle" where
  label _ = "Axis label angle"
instance isColorSchemeReversedHasLabel ∷ HasLabel "isColorSchemeReversed" where
  label _ = "Reversed color scheme"
instance optionalMarkersHasLabel ∷ HasLabel "optionalMarkers" where
  label _ = "Optional Markers"
instance orderHasLabel ∷ HasLabel "order" where
  label _ = "Order"
instance alignHasLabel ∷ HasLabel "align" where
  label _ = "Align"
instance formatterHasLabel ∷ HasLabel "formatter" where
  label _ = "Value formatter"
instance circularHasLabel ∷ HasLabel "circular" where
  label _ = "Circular layout"
instance colorSchemeHasLabel ∷ HasLabel "colorScheme" where
  label _ = "Color scheme"

data SetF a = Set String a
derive instance functorSetF ∷ Functor SetF
data MinMaxF a = Max String a | Min String a
derive instance functorMinMaxF ∷ Functor MinMaxF
data ToggleF a = Toggle a
derive instance functorToggleF ∷ Functor ToggleF
data ChooseF val a = Choose val a
derive instance functorChooseF ∷ Functor (ChooseF a)


_size = SProxy ∷ SProxy "size"
_uriString = SProxy ∷ SProxy "uriString"
_uri = SProxy ∷ SProxy "uri"
_min = SProxy ∷ SProxy "min"
_max = SProxy ∷ SProxy "max"
_isSmooth = SProxy ∷ SProxy "isSmooth"
_isStacked = SProxy ∷ SProxy "isStacked"
_osm = SProxy ∷ SProxy "osm"
_axisLabelAngle = SProxy ∷ SProxy "axisLabelAngle"
_val = SProxy ∷ SProxy "val"
_isColorSchemeReversed = SProxy ∷ SProxy "isColorSchemeReversed"
_colorScheme = SProxy ∷ SProxy "colorScheme"
_optionalMarkers = SProxy ∷ SProxy "optionalMarkers"
_order = SProxy ∷ SProxy "order"
_align = SProxy ∷ SProxy "align"
_circular = SProxy ∷ SProxy "circular"
_formatter = SProxy ∷ SProxy "formatter"

renderChoose
  ∷ ∀ r1 r2 s q1 q2 a
  . OptionVal a
  ⇒ RowCons s a r1 r2
  ⇒ RowCons s (FProxy (ChooseF a)) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Select a
  → Record r1
  → H.ComponentHTML (VariantF q2)
renderChoose proxy (Select { options, value }) state =
  let
    len = Arr.length options
    renderedOptions = map renderOption options
  in
    HH.select
      [ HP.classes [ CN.formControl ]
      , HP.disabled $ len < 1
      , HE.onSelectedIndexChange \ix →
          Arr.index options ix <#> \val → inj proxy $ H.action $ Choose val
      ]
      $ map renderOption options
  where
  renderOption option =
    HH.option
      [ HP.selected $ Just option ≡ value ]
      [ HH.text $ stringVal option ]

evalChoose
  ∷ ∀ r1 r2 s q m a
  . OptionVal a
  ⇒ RowCons s a r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → ChooseF a
  ~> H.ComponentDSL (Record r2) q (Record r2) m
evalChoose proxy = case _ of
  Choose a next → do
    H.modify $ prop proxy .~ a
    st ← H.get
    H.raise st
    pure next

type OsmURIState =
  { uri ∷ URIRef
  , uriString ∷ String
  }

type MinMaxState =
  { min ∷ Number
  , max ∷ Number
  }

renderOsmURI
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s OsmURIState r1 r2
  ⇒ RowCons s (FProxy SetF) q1 q2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
renderOsmURI proxy state =
  HH.div [ HP.classes [ CSS.axisLabelParam ] ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Open Street Map URI" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ state ^. lens
        , ARIA.label "Open Street Map URI"
        , HE.onValueInput \s → Just $ inj proxy $ H.action $ Set s
        ]
    ]
  where
  lens ∷ Lens' (Record r2) String
  lens = prop proxy ∘ prop _uriString

evalOsmURI
  ∷ ∀ r1 r2 s q m
  . RowCons s OsmURIState r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → SetF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
evalOsmURI proxy = case _ of
  Set s next → do
    let
      oRx = URX.unsafeRegex "{" RXF.global
      cRx = URX.unsafeRegex "}" RXF.global
      replaced = RX.replace oRx "%7B" $ RX.replace cRx "%7D" s
    H.modify $  case runParseURIRef replaced of
      Left e → \st →
        st # _osmURIString .~ printURIRef (st ^. _osmURI)
      Right uri →
        ( _osmURI .~ onURIRef decodeURIComponent uri )
        ∘ ( _osmURIString .~ s )
    st ← H.get
    H.raise st
    pure next
  where
  _osmURIString ∷ Lens' (Record r2) String
  _osmURIString = prop proxy ∘ prop _uriString

  _osmURI ∷ Lens' (Record r2) URIRef
  _osmURI = prop proxy ∘ prop _uri

renderMinMax
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s MinMaxState r1 r2
  ⇒ RowCons s (FProxy MinMaxF) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
renderMinMax proxy state =
  row
  [ HH.div
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    ]
    [ HH.label
        [ HP.classes [ B.controlLabel ] ]
        [ HH.text $ "Minimum " <> label proxy ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state ^. _minSize
        , ARIA.label $  "Min " <> label proxy
        , HE.onValueInput \s → Just $ inj proxy $ H.action $ Min s
        ]
    ]
  , HH.div
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text $ "Maximum " <> label proxy ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state ^. _maxSize
        , ARIA.label $ "Max " <> label proxy
        , HE.onValueInput \s → Just $ inj (SProxy ∷ SProxy s) $ H.action $ Max s
        ]
    ]
  ]
  where
  _minSize ∷ Lens' (Record r2) Number
  _minSize = prop proxy ∘ prop _min

  _maxSize ∷ Lens' (Record r2) Number
  _maxSize = prop proxy ∘ prop _max


evalMinMax
  ∷ ∀ r1 r2 s m q
  . RowCons s MinMaxState r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → MinMaxF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
evalMinMax proxy = case _ of
  Min str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st → st
        # ( _minSize .~ fl )
        ∘ ( _maxSize .~ if (st ^. _maxSize) > fl then st ^. _maxSize else fl )
      st ← H.get
      H.raise st
    pure next
  Max str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st → st
        # ( _maxSize .~ fl )
        ∘ ( _minSize .~ if (st ^. _minSize) < fl then st ^. _minSize else fl )
      st ← H.get
      H.raise st
    pure next
  where
  _minSize ∷ Lens' (Record r2) Number
  _minSize = prop (SProxy ∷ SProxy s) ∘ prop _min

  _maxSize ∷ Lens' (Record r2) Number
  _maxSize = prop (SProxy ∷ SProxy s) ∘ prop _max

renderToggle
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s Boolean r1 r2
  ⇒ RowCons s (FProxy ToggleF) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
renderToggle proxy state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked $ state ^. lens
            , ARIA.label $ label proxy
            , HE.onValueInput \_ → Just $ inj proxy $ H.action $ Toggle
            ]
        , HH.text $ label proxy
        ]
    ]
  where
  lens ∷ Lens' (Record r2) Boolean
  lens = prop proxy

evalToggle
  ∷ ∀ r1 r2 s m q
  . RowCons s Boolean r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → ToggleF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
evalToggle _ = case _ of
  Toggle next → do
    H.modify $ _toggle %~ not
    st ← H.get
    H.raise st
    pure next
  where
  _toggle ∷ Lens' (Record r2) Boolean
  _toggle = prop (SProxy ∷ SProxy s)

renderNum
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s Number r1 r2
  ⇒ RowCons s (FProxy SetF) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
renderNum proxy state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label
        [ HP.classes [ CN.controlLabel ] ]
        [ HH.text $ label proxy ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ show $ state ^. lens
        , ARIA.label $ label proxy
        , HE.onValueInput \s → Just $ inj proxy $ H.action $ Set s
        ]
    ]
  where
  lens ∷ Lens' (Record r2) Number
  lens = prop proxy

renderStr
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s String r1 r2
  ⇒ RowCons s (FProxy SetF) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
renderStr proxy state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label
        [ HP.classes [ CN.controlLabel ] ]
        [ HH.text $ label proxy ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ state ^. lens
        , ARIA.label $ label proxy
        , HE.onValueInput \s → Just $ inj proxy $ H.action $ Set s
        ]
    ]
  where
  lens ∷ Lens' (Record r2) String
  lens = prop proxy

evalNum
  ∷ ∀ r1 r2 s q m
  . RowCons s Number r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → SetF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
evalNum proxy = case _ of
  Set str next → do
    let fl = readFloat str
    unless (isNaN fl)
      $ H.modify $ prop proxy .~ fl
    st ← H.get
    H.raise st
    pure next

evalStr
  ∷ ∀ r1 r2 s q m
  . RowCons s String r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → SetF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
evalStr proxy = case _ of
  Set str next → do
    H.modify $ prop proxy .~ str
    st ← H.get
    H.raise st
    pure next


type ResetF m r = VariantF ( reset ∷ FProxy (Tuple m) | r)

_reset = SProxy ∷ SProxy "reset"

evalReset
  ∷ ∀ s m q i
  . Tuple s
  ~> H.ComponentDSL s q i m
evalReset = case _ of
  Tuple st next → do
    H.put st
    pure next

type AuxComponent a b m = H.Component HH.HTML (ResetF b a) (Maybe b) b m

type GeoHeatmapState = { osm ∷ OsmURIState }
type GeoHeatmapF = ( osm ∷ FProxy SetF )

geoHeatmap ∷ ∀ m. AuxComponent GeoHeatmapF GeoHeatmapState m
geoHeatmap = H.component
  { initialState: const { osm: { uri: M.osmURI, uriString: printURIRef M.osmURI } }
  , render: \state → row [ renderOsmURI _osm state ]
  , eval: case_
    # on _osm (evalOsmURI _osm)
    # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type GeoMarkerState = { osm ∷ OsmURIState, size ∷ MinMaxState }
type GeoMarkerF = ( osm ∷ FProxy SetF, size ∷ FProxy MinMaxF )

geoMarker ∷ ∀ m. AuxComponent GeoMarkerF GeoMarkerState m
geoMarker = H.component
  { initialState: const { osm: { uri: M.osmURI
                               , uriString: printURIRef M.osmURI
                               }
                        , size: { min: 10.0
                                , max: 50.0
                                }
                        }
  , render: \state → HH.div_
     [ HH.hr_
     , renderMinMax _size state
     , HH.hr_
     , renderOsmURI _osm state
     ]
  , eval: case_
      # on _size (evalMinMax _size)
      # on _osm (evalOsmURI _osm)
      # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type AreaState =
  { isStacked ∷ Boolean
  , isSmooth ∷ Boolean
  , axisLabelAngle ∷ Number
  , size ∷ Number
  }

type AreaF =
  ( isStacked ∷ FProxy ToggleF
  , isSmooth ∷ FProxy ToggleF
  , axisLabelAngle ∷ FProxy SetF
  , size ∷ FProxy SetF
  )

area ∷ ∀ m. AuxComponent AreaF AreaState m
area = H.component
  { initialState: const { isStacked: false
                        , isSmooth: false
                        , size: 10.0
                        , axisLabelAngle: 0.0
                        }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderToggle _isStacked state
          , renderToggle _isSmooth state
          ]
    , HH.hr_
    , row [ renderNum _axisLabelAngle state
          , renderNum _size state
          ]
    ]
  , eval: case_
    # on _isStacked (evalToggle _isStacked)
    # on _isSmooth (evalToggle _isSmooth)
    # on _axisLabelAngle (evalNum _axisLabelAngle)
    # on _size (evalNum _size)
    # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type BarState = { axisLabelAngle ∷ Number }
type BarF = ( axisLabelAngle ∷ FProxy SetF )

bar ∷ ∀ m. AuxComponent BarF BarState m
bar = H.component
  { initialState: const { axisLabelAngle: 0.0 }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderNum _axisLabelAngle state ]
    ]
  , eval: case_
    # on _axisLabelAngle (evalNum _axisLabelAngle)
    # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type FunnelF = ( order ∷ FProxy (ChooseF Sort), align ∷ FProxy (ChooseF Align) )
type FunnelState = { order ∷ Sort, align ∷ Align }

funnel ∷ ∀ m. AuxComponent FunnelF FunnelState m
funnel = H.component
  { initialState: const { order: Asc, align: LeftAlign }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderChoose _order sortSelect state
          , renderChoose _align alignSelect state
          ]
    ]
  , eval: case_
    # on _order (evalChoose _order)
    # on _align (evalChoose _align)
    # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type GraphF = ( size ∷ FProxy MinMaxF, circular ∷ FProxy ToggleF )
type GraphState = { size ∷ MinMaxState, circular ∷ Boolean }

graph ∷ ∀ m. AuxComponent GraphF GraphState m
graph = H.component
  { initialState: const { circular: false, size: { min: 10.0, max: 50.0 } }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderToggle _circular state ]
    , HH.hr_
    , renderMinMax _size state
    ]
  , eval: case_
    # on _circular (evalToggle _circular)
    # on _size (evalMinMax _size)
    # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type HeatmapF =
  ( colorScheme ∷ FProxy (ChooseF ColorScheme)
  , isColorSchemeReversed ∷ FProxy ToggleF
  , val ∷ FProxy MinMaxF
  )
type HeatmapState =
  { colorScheme ∷ ColorScheme
  , isColorSchemeReversed ∷ Boolean
  , val ∷ MinMaxState
  }

heatmap ∷ ∀ m. AuxComponent HeatmapF HeatmapState m
heatmap = H.component
  { initialState:const { colorScheme: RedToBlue
                       , isColorSchemeReversed: false
                       , val: { min: 0.0, max: 50.0 }
                       }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderChoose _colorScheme colorSchemeSelect state
          , renderToggle _isColorSchemeReversed state
          ]
    , HH.hr_
    , renderMinMax _val state
    ]
  , eval: case_
    # on _val (evalMinMax _val)
    # on _colorScheme (evalChoose _colorScheme)
    # on _isColorSchemeReversed (evalToggle _isColorSchemeReversed)
    # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type LineF =
  ( optionalMarkers ∷ FProxy ToggleF
  , size ∷ FProxy MinMaxF
  , axisLabelAngle ∷ FProxy SetF
  )
type LineState =
  { optionalMarkers ∷ Boolean
  , size ∷ MinMaxState
  , axisLabelAngle ∷ Number
  }

line ∷ ∀ m. AuxComponent LineF LineState m
line = H.component
  { initialState: const { optionalMarkers: false
                        , size: { min: 10.0, max: 50.0 }
                        , axisLabelAngle: 0.0
                        }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderToggle _optionalMarkers state ]
    , HH.hr_
    , renderMinMax _size state
    , HH.hr_
    , row [ renderNum _axisLabelAngle state ]
    ]
  , eval: case_
    # on _optionalMarkers (evalToggle _optionalMarkers)
    # on _size (evalMinMax _size)
    # on _axisLabelAngle (evalNum _axisLabelAngle)
    # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type MetricF = ( formatter ∷ FProxy SetF )
type MetricState = { formatter ∷ String }

metric ∷ ∀ m. AuxComponent MetricF MetricState m
metric = H.component
  { initialState: const { formatter: "" }
  , render: \state → HH.div_
    [ HH.hr_
    , renderStr _formatter state
    , HH.hr_
    , HH.div_
      [ HH.p_ [ HH.text "Value between \"{{\" and \"}}\" will be replaced by following rules" ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{0}}"]
        , HH.text " rounds to the closest integer"
        ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{0,0}}"]
        , HH.text " rounds to the closest integer and adds thousands delimiters"
        ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{000}}" ]
        , HH.text " adds leading zeros to the value"
        ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{0a}}" ]
        , HH.text " adds an abbreviation"
        ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{0.000}}" ]
        , HH.text " leaves three numbers after dot or adds up to three trailing zeros"
        ]
      , HH.p_
        [ HH.a [ HP.href "https://github.com/slamdata/purescript-formatters" ]
            [ HH.text "Complete documentation"
            ]
        ]
      ]
    ]
  , eval: case_ # on _formatter (evalStr _formatter) # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type PunchCardF = ( size ∷ FProxy MinMaxF, circular ∷ FProxy ToggleF )
type PunchCardState = { size ∷ MinMaxState, circular ∷ Boolean }

punchCard ∷ ∀ m. AuxComponent PunchCardF PunchCardState m
punchCard = H.component
  { initialState: const { circular: false, size: { min: 10.0, max: 50.0 } }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderToggle _circular state ]
    , HH.hr_
    , renderMinMax _size state
    ]
  , eval: case_
    # on _circular (evalToggle _circular)
    # on _size (evalMinMax _size)
    # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

type ScatterF = ( size ∷ FProxy MinMaxF )
type ScatterState = { size ∷ MinMaxState }

scatter ∷ ∀ m. AuxComponent ScatterF ScatterState m
scatter = H.component
  { initialState: const { size: { min: 10.0, max: 50.0 } }
  , render: \state → HH.div_
    [ HH.hr_
    , renderMinMax _size state
    ]
  , eval: case_ # on _size (evalMinMax _size) # on _reset evalReset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }

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

type AuxState =
  ( geoHeatmap ∷ GeoHeatmapState
  , geoMarker ∷ GeoMarkerState
  , area ∷ AreaState
  , bar ∷ BarState
  , funnel ∷ FunnelState
  , graph ∷ GraphState
  , heatmap ∷ HeatmapState
  , line ∷ LineState
  , metric ∷ MetricState
  , punchCard ∷ PunchCardState
  , scatter ∷ ScatterState
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
