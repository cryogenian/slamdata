module SlamData.Workspace.Card.Setups.Viz.Auxiliary where

import SlamData.Prelude

import Data.Array as Arr
import Data.URI (URIRef, runParseURIRef, printURIRef)
import Data.Symbol (class IsSymbol)
import Data.Functor.Variant (FProxy, VariantF, inj, case_, on)
import Data.Lens ((^.), Lens', (.~), (%~))
import Data.Lens.Record (prop)
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX

import Global (decodeURIComponent, readFloat, isNaN)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.Setups.Chart.ColorScheme (ColorScheme(..), colorSchemeSelect)
import SlamData.Form.Select (class OptionVal, Select(..), stringVal)
import SlamData.Common.Sort (sortSelect, Sort(..))
import SlamData.Common.Align (alignSelect, Align(..))
import SlamData.Monad (Slam)
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
instance minValueHasLabel ∷ HasLabel "minValue" where
  label _ = "Minimum value"
instance maxValueHasLabel ∷ HasLabel "maxValue" where
  label _ = "Maximum value"
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

data Message m = Updated m

data SetF a = Set String a
derive instance functorSetF ∷ Functor SetF
data MinMaxF a = Max String a | Min String a
derive instance functorMinMaxF ∷ Functor MinMaxF
data ToggleF a = Toggle a
derive instance functorToggleF ∷ Functor ToggleF
data ChooseF val a = Choose val a
derive instance functorChooseF ∷ Functor (ChooseF a)


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
  ~> H.ComponentDSL (Record r2) q (Message (Record r2)) m
evalChoose proxy = case _ of
  Choose a next → do
    H.modify $ prop proxy .~ a
    st ← H.get
    H.raise $ Updated st
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
  lens = prop proxy ∘ prop (SProxy ∷ SProxy "uriString")

evalOsmURI
  ∷ ∀ r1 r2 s q m
  . RowCons s OsmURIState r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → SetF
  ~> H.ComponentDSL (Record r2) q (Message (Record r2)) m
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
    H.raise $ Updated st
    pure next
  where
  _osmURIString ∷ Lens' (Record r2) String
  _osmURIString = prop proxy ∘ prop (SProxy ∷ SProxy "uriString")

  _osmURI ∷ Lens' (Record r2) URIRef
  _osmURI = prop proxy ∘ prop (SProxy ∷ SProxy "uri")

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
  _minSize = prop proxy ∘ prop (SProxy ∷ SProxy "min")

  _maxSize ∷ Lens' (Record r2) Number
  _maxSize = prop proxy ∘ prop (SProxy ∷ SProxy "max")


evalMinMax
  ∷ ∀ r1 r2 s m q
  . RowCons s MinMaxState r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → MinMaxF
  ~> H.ComponentDSL (Record r2) q (Message (Record r2)) m
evalMinMax proxy = case _ of
  Min str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st → st
        # ( _minSize .~ fl )
        ∘ ( _maxSize .~ if (st ^. _maxSize) > fl then st ^. _maxSize else fl )
      st ← H.get
      H.raise $ Updated st
    pure next
  Max str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st → st
        # ( _maxSize .~ fl )
        ∘ ( _minSize .~ if (st ^. _minSize) < fl then st ^. _minSize else fl )
      st ← H.get
      H.raise $ Updated st
    pure next
  where
  _minSize ∷ Lens' (Record r2) Number
  _minSize = prop (SProxy ∷ SProxy s) ∘ prop (SProxy ∷ SProxy "min")

  _maxSize ∷ Lens' (Record r2) Number
  _maxSize = prop (SProxy ∷ SProxy s) ∘ prop (SProxy ∷ SProxy "max")

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
  ~> H.ComponentDSL (Record r2) q (Message (Record r2)) m
evalToggle _ = case _ of
  Toggle next → do
    H.modify $ _toggle %~ not
    st ← H.get
    H.raise $ Updated st
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
  ~> H.ComponentDSL (Record r2) q (Message (Record r2)) m
evalNum proxy = case _ of
  Set str next → do
    let fl = readFloat str
    unless (isNaN fl)
      $ H.modify $ prop proxy .~ fl
    st ← H.get
    H.raise $ Updated st
    pure next

evalStr
  ∷ ∀ r1 r2 s q m
  . RowCons s String r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → SetF
  ~> H.ComponentDSL (Record r2) q (Message (Record r2)) m
evalStr proxy = case _ of
  Set str next → do
    H.modify $ prop proxy .~ str
    st ← H.get
    H.raise $ Updated st
    pure next

type TstState = { isStacked ∷ Boolean, isSmooth ∷ Boolean }
type TstQuery = VariantF ( isStacked ∷ FProxy ToggleF, isSmooth ∷ FProxy ToggleF )

render ∷ TstState → H.ComponentHTML TstQuery
render state =
  HH.div_
  [ renderToggle (SProxy ∷ SProxy "isSmooth") state
  , renderToggle (SProxy ∷ SProxy "isStacked") state
  ]

tstEval ∷ TstQuery ~> H.ComponentDSL TstState TstQuery (Message TstState) Slam
tstEval = case_
  # on (SProxy ∷ SProxy "isSmooth") (evalToggle (SProxy ∷ SProxy "isSmooth"))
  # on (SProxy ∷ SProxy "isStacked") (evalToggle (SProxy ∷ SProxy "isStacked"))

geoHeatmap
  ∷ H.Component
      HH.HTML
      (VariantF (osm ∷ FProxy SetF))
      Unit
      (Message {osm ∷ OsmURIState})
      Slam
geoHeatmap = H.component
  { initialState: const { osm: { uri: M.osmURI, uriString: printURIRef M.osmURI } }
  , render: \state → row [ renderOsmURI (SProxy ∷ SProxy "osm") state ]
  , eval: case_ # on (SProxy ∷ SProxy "osm") (evalOsmURI (SProxy ∷ SProxy "osm"))
  , receiver: const Nothing
  }

geoMarker
  ∷ H.Component
      HH.HTML
      (VariantF (osm ∷ FProxy SetF, size ∷ FProxy MinMaxF))
      Unit
      (Message {osm ∷ OsmURIState, size ∷ MinMaxState})
      Slam
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
     , renderMinMax (SProxy ∷ SProxy "size") state
     , HH.hr_
     , renderOsmURI (SProxy ∷ SProxy "osm") state
     ]
  , eval: case_
      # on (SProxy ∷ SProxy "size") (evalMinMax (SProxy ∷ SProxy "size"))
      # on (SProxy ∷ SProxy "osm") (evalOsmURI (SProxy ∷ SProxy "osm"))
  , receiver: const Nothing
  }

area
  ∷ H.Component
      HH.HTML
      ( VariantF ( isStacked ∷ FProxy ToggleF
                 , isSmooth ∷ FProxy ToggleF
                 , axisLabelAngle ∷ FProxy SetF
                 , size ∷ FProxy SetF
                 ) )
      Unit
      ( Message { isStacked ∷ Boolean
                , isSmooth ∷ Boolean
                , axisLabelAngle ∷ Number
                , size ∷ Number
                } )
      Slam
area = H.component
  { initialState: const { isStacked: false
                        , isSmooth: false
                        , size: 10.0
                        , axisLabelAngle: 0.0
                        }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderToggle (SProxy ∷ SProxy "isStacked") state
          , renderToggle (SProxy ∷ SProxy "isSmooth") state
          ]
    , HH.hr_
    , row [ renderNum (SProxy ∷ SProxy "axisLabelAngle") state
          , renderNum (SProxy ∷ SProxy "size") state
          ]
    ]
  , eval: case_
    # on (SProxy ∷ SProxy "isStacked") (evalToggle (SProxy ∷ SProxy "isStacked"))
    # on (SProxy ∷ SProxy "isSmooth") (evalToggle (SProxy ∷ SProxy "isSmooth"))
    # on (SProxy ∷ SProxy "axisLabelAngle") (evalNum (SProxy ∷ SProxy "axisLabelAngle"))
    # on (SProxy ∷ SProxy "size") (evalNum (SProxy ∷ SProxy "size"))
  , receiver: const Nothing
  }

bar
  ∷ H.Component
      HH.HTML
      (VariantF (axisLabelAngle ∷ FProxy SetF))
      Unit
      (Message {axisLabelAngle ∷ Number})
      Slam
bar = H.component
  { initialState: const { axisLabelAngle: 0.0 }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderNum (SProxy ∷ SProxy "axisLabelAngle") state ]
    ]
  , eval: case_
    # on (SProxy ∷ SProxy "axisLabelAngle") (evalNum (SProxy ∷ SProxy "axisLabelAngle"))
  , receiver: const Nothing
  }


funnel
  ∷ H.Component
      HH.HTML
      (VariantF ( order ∷ FProxy (ChooseF Sort)
                , align ∷ FProxy (ChooseF Align)
                ))
      Unit
      (Message { order ∷ Sort
               , align ∷ Align
               })
      Slam
funnel = H.component
  { initialState: const { order: Asc, align: LeftAlign }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderChoose (SProxy ∷ SProxy "order") sortSelect state
          , renderChoose (SProxy ∷ SProxy "align") alignSelect state
          ]
    ]
  , eval: case_
    # on (SProxy ∷ SProxy "order") (evalChoose (SProxy ∷ SProxy "order"))
    # on (SProxy ∷ SProxy "align") (evalChoose (SProxy ∷ SProxy "align"))
  , receiver: const Nothing
  }

graph
  ∷ H.Component
      HH.HTML
      ( VariantF ( size ∷ FProxy MinMaxF, circular ∷ FProxy ToggleF ) )
      Unit
      ( Message { size ∷ MinMaxState, circular ∷ Boolean } )
      Slam
graph = H.component
  { initialState: const { circular: false, size: { min: 10.0, max: 50.0 } }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderToggle (SProxy ∷ SProxy "circular") state ]
    , HH.hr_
    , renderMinMax (SProxy ∷ SProxy "size") state
    ]
  , eval: case_
    # on (SProxy ∷ SProxy "circular") (evalToggle (SProxy ∷ SProxy "circular"))
    # on (SProxy ∷ SProxy "size") (evalMinMax (SProxy ∷ SProxy "size"))
  , receiver: const Nothing
  }

heatmap
  ∷ H.Component
      HH.HTML
      ( VariantF ( colorScheme ∷ FProxy (ChooseF ColorScheme)
                 , isColorSchemeReversed ∷ FProxy ToggleF
                 , val ∷ FProxy MinMaxF
                 ) )
      Unit
      ( Message { colorScheme ∷ ColorScheme
                , isColorSchemeReversed ∷ Boolean
                , val ∷ { min ∷ Number, max ∷ Number }
                } )
      Slam
heatmap = H.component
  { initialState:const { colorScheme: RedToBlue
                       , isColorSchemeReversed: false
                       , val: { min: 0.0, max: 50.0 }
                       }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderChoose (SProxy ∷ SProxy "colorScheme") colorSchemeSelect state
          , renderToggle (SProxy ∷ SProxy "isColorSchemeReversed") state
          ]
    , HH.hr_
    , renderMinMax (SProxy ∷ SProxy "val") state
    ]
  , eval: case_
    # on (SProxy ∷ SProxy "val") (evalMinMax (SProxy ∷ SProxy "val"))
    # on (SProxy ∷ SProxy "colorScheme") (evalChoose (SProxy ∷ SProxy "colorScheme"))
    # on (SProxy ∷ SProxy "isColorSchemeReversed") (evalToggle (SProxy ∷ SProxy "isColorSchemeReversed"))
  , receiver: const Nothing
  }

line
  ∷ H.Component
      HH.HTML
      ( VariantF ( optionalMarkers ∷ FProxy ToggleF
                 , size ∷ FProxy MinMaxF
                 , axisLabelAngle ∷ FProxy SetF
                 ) )
      Unit
      ( Message { optionalMarkers ∷ Boolean
                , size ∷ MinMaxState
                , axisLabelAngle ∷ Number
                } )

      Slam
line = H.component
  { initialState: const { optionalMarkers: false
                        , size: { min: 10.0, max: 50.0 }
                        , axisLabelAngle: 0.0
                        }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderToggle (SProxy ∷ SProxy "optionalMarkers") state ]
    , HH.hr_
    , renderMinMax (SProxy ∷ SProxy "size") state
    , HH.hr_
    , row [ renderNum (SProxy ∷ SProxy "axisLabelAngle") state ]
    ]
  , eval: case_
    # on (SProxy ∷ SProxy "optionalMarkers") (evalToggle (SProxy ∷ SProxy "optionalMarkers"))
    # on (SProxy ∷ SProxy "size") (evalMinMax (SProxy ∷ SProxy "size"))
    # on (SProxy ∷ SProxy "axisLabelAngle") (evalNum (SProxy ∷ SProxy "axisLabelAngle"))
  , receiver: const Nothing
  }


metric
  ∷ H.Component
      HH.HTML
      ( VariantF ( formatter ∷ FProxy SetF ) )
      Unit
      ( Message { formatter ∷ String } )
      Slam
metric = H.component
  { initialState: const { formatter: "" }
  , render: \state → HH.div_
    [ HH.hr_
    , renderStr (SProxy ∷ SProxy "formatter") state
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
  , eval: case_ # on (SProxy ∷ SProxy "formatter") (evalStr (SProxy ∷ SProxy "formatter"))
  , receiver: const Nothing
  }

punchCard
  ∷ H.Component
      HH.HTML
      ( VariantF ( size ∷ FProxy MinMaxF, circular ∷ FProxy ToggleF ) )
      Unit
      ( Message { size ∷ MinMaxState, circular ∷ Boolean } )
      Slam
punchCard = H.component
  { initialState: const { circular: false, size: { min: 10.0, max: 50.0 } }
  , render: \state → HH.div_
    [ HH.hr_
    , row [ renderToggle (SProxy ∷ SProxy "circular") state ]
    , HH.hr_
    , renderMinMax (SProxy ∷ SProxy "size") state
    ]
  , eval: case_
    # on (SProxy ∷ SProxy "circular") (evalToggle (SProxy ∷ SProxy "circular"))
    # on (SProxy ∷ SProxy "size") (evalMinMax (SProxy ∷ SProxy "size"))
  , receiver: const Nothing
  }

scatter
  ∷ H.Component
      HH.HTML
      ( VariantF ( size ∷ FProxy MinMaxF ) )
      Unit
      ( Message { size ∷ MinMaxState } )
      Slam
scatter = H.component
  { initialState: const { size: { min: 10.0, max: 50.0 } }
  , render: \state → HH.div_
    [ HH.hr_
    , renderMinMax (SProxy ∷ SProxy "size") state
    ]
  , eval: case_ # on (SProxy ∷ SProxy "size") (evalMinMax (SProxy ∷ SProxy "size"))
  , receiver: const Nothing
  }
