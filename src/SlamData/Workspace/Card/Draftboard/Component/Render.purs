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

module SlamData.Workspace.Card.Draftboard.Component.Render
  ( render
  ) where

import SlamData.Prelude
import CSS as C
import Data.Array as Array
import Data.List (List)
import Data.Ratio as Ratio
import Data.Rational (Rational, (%))
import Data.Rational as Rational
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.HTML.CSS as HC
import Math as Math
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Draftboard.Component.Common (DraftboardHTML, rootRef)
import SlamData.Workspace.Card.Draftboard.Component.Query (Query(..))
import SlamData.Workspace.Card.Draftboard.Component.State (State, MoveLocation(..), SplitLocation)
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.DeckId (DeckId)
import Utils.DOM as DOM

render ∷ CardOptions → State → DraftboardHTML
render opts st =
  HH.div
    [ HP.classes
        ([ HH.ClassName "sd-draftboard" ]
         <> (HH.ClassName "splitting" <$ guard (isJust st.splitOpts))
         <> (HH.ClassName "sizing" <$ guard (isJust st.resizeLocation)))
    ]
    [ renderOuterEdge "top" Orn.Vertical Layout.SideA
    , renderOuterEdge "left" Orn.Horizontal Layout.SideA
    , renderOuterEdge "right" Orn.Horizontal Layout.SideB
    , renderOuterEdge "bottom" Orn.Vertical Layout.SideB
    , HH.div
        [ HP.classes [ HH.ClassName "sd-draftboard-root" ]
        , HP.ref rootRef
        ]
        [ renderLayout opts st st.cellLayout st.edgeLayout
        , maybe (HH.text "") renderGuide st.splitLocation
        , maybe (HH.text "") renderMoving st.moveLocation
        ]
    ]

renderOuterEdge ∷ String → Orn.Orientation → Layout.SplitBias → DraftboardHTML
renderOuterEdge edge orn bias =
  HH.button
    [ HP.classes
        [ HH.ClassName "sd-draftboard-edge"
        , HH.ClassName ("sd-draftboard-edge-" <> edge)
        ]
    , HE.onMouseDown (HE.input (\e → right ∘ SplitStart orn bias false e))
    ]
    [ HH.span
        [ HP.classes [ HH.ClassName "sd-draftboard-edge-mid" ]
        , HE.onMouseDown \e →
            Just (right (H.action (SplitStart orn bias true e)))
        ]
        [ ]
    ]

renderGuide ∷ SplitLocation → DraftboardHTML
renderGuide sp =
  HH.div
    [ HP.classes
        ([ HH.ClassName "sd-draftboard-guide"
         , HH.ClassName (Orn.toString sp.orientation)
         ] <> (HH.ClassName "invalid" <$ guard (not sp.valid)))
    , HC.style do
        C.top (C.px sp.y)
        C.left (C.px sp.x)
        case sp.orientation of
          Orn.Horizontal → C.height (C.px sp.z)
          Orn.Vertical   → C.width (C.px sp.z)
    ]
    [ renderGuideLabel sp.ratio ]

renderGuideLabel ∷ Rational → DraftboardHTML
renderGuideLabel ratio =
  let ratio' = unRational ratio in
  HH.span
    [ HP.classes
        [ HH.ClassName "sd-draftboard-guide-label"
        , if ratio > 1%2
            then HH.ClassName "trailing"
            else HH.ClassName "leading"
        ]
    ]
    [ HH.text (show (Ratio.numerator ratio') <> "/" <> show (Ratio.denominator ratio')) ]

renderMoving ∷ MoveLocation → DraftboardHTML
renderMoving = case _ of
  Floating x y →
    HH.div
      [ HP.classes
          [ HH.ClassName "sd-draftboard-moving-icon"
          , HH.ClassName "floating"
          ]
      , HC.style do
          C.left (C.px x)
          C.top (C.px y)
      ]
      [ ]
  Move { rect } →
    HH.div
      [ HP.classes
          [ HH.ClassName "sd-draftboard-moving-icon"
          , HH.ClassName "move"
          ]
      , HC.style do
          C.top (C.px rect.top)
          C.left (C.px rect.left)
          C.width (C.px rect.width)
          C.height (C.px rect.height)
      ]
      [ ]
  Group { rect } orn side →
    HH.div
      [ HP.classes
          [ HH.ClassName "sd-draftboard-moving-icon"
          , HH.ClassName "group"
          , HH.ClassName (Orn.toString orn)
          , HH.ClassName case side of
              Layout.SideA → "side-a"
              Layout.SideB → "side-b"
          ]
      , HC.style do
          C.top (C.px rect.top)
          C.left (C.px rect.left)
          C.width (C.px rect.width)
          C.height (C.px rect.height)
      ]
      [ ]

renderLayout ∷ CardOptions → State → List (Layout.Cell (Maybe DeckId) Number) → List (Layout.Edge Number) → DraftboardHTML
renderLayout opts st cells edges =
  HH.div
    [ HP.classes [ HH.ClassName "sd-draftboard-layout" ] ] $
    map (renderCell opts st) (Array.fromFoldable cells) <>
    map (renderEdge st) (Array.fromFoldable edges)

renderCell ∷ CardOptions → State → Layout.Cell (Maybe DeckId) Number → DraftboardHTML
renderCell opts st { cursor, value, rect } =
  HH.div
    ([ HP.classes
         [ HH.ClassName "sd-draftboard-cell"
         , HH.ClassName if isJust value then "filled" else "empty"
         ]
     , HC.style do
         C.top (C.px rect.top)
         C.left (C.px rect.left)
         C.width (C.px rect.width)
         C.height (C.px rect.height)
     ])
    [ HH.div
        [ HP.classes [ HH.ClassName "sd-draftboard-cell-content" ] ]
        case value of
          Just deckId →
            [ HH.slot deckId (mkDeckComponent deckId) unit case _ of
                DCQ.GrabbedDeck ev →
                  Just (right (H.action (GrabStart deckId ev)))
            ]
          _ →
            [ HH.div
                [ HP.classes [ HH.ClassName "sd-draftboard-cell-empty" ] ]
                [ HH.button
                    [ HP.classes [ HH.ClassName "insert-cell" ]
                    , HP.title "Insert deck"
                    , ARIA.label "Insert deck"
                    , HE.onClick (HE.input_ (right ∘ AddDeck cursor))
                    , HC.style do
                        let
                          size = Math.min 140.0 (Math.min rect.width rect.height)
                        C.top (C.pct 50.0)
                        C.left (C.pct 50.0)
                        C.marginTop (C.px (size / -2.0))
                        C.marginLeft (C.px (size / -2.0))
                        C.width (C.px size)
                        C.height (C.px size)
                    ]
                    [ HH.span_ [] ]
                , HH.button
                    [ HP.classes [ HH.ClassName "delete-cell" ]
                    , HP.title "Delete cell"
                    , ARIA.label "Delete cell"
                    , HE.onClick (HE.input_ (right ∘ DeleteCell cursor))
                    ]
                    [ HH.text "×"]
                ]
            ]
    ]
  where
  mkDeckComponent deckId =
    opts.deckComponent
      { accessType: opts.deck.accessType
      , cursor: opts.cursor
      , displayCursor: opts.displayCursor
      , deckId
      }

renderEdge ∷ State → Layout.Edge Number → DraftboardHTML
renderEdge st edge@{ orientation, vect } =
  if fromMaybe false ((\loc → Layout.eqEdge edge loc.edge) <$> st.resizeLocation)
    then
      st.resizeLocation # maybe (HH.text "") \loc →
      let ord = compare loc.ratio edge.ratio in
      HH.button
        [ HP.classes
            ([ HH.ClassName "sd-draftboard-split-edge"
             , HH.ClassName (Orn.toString orientation)
             , HH.ClassName "sizing"
             , HH.ClassName
                 case ord of
                   LT → "lt"
                   GT → "gt"
                   EQ → "eq"
             ] <> (HH.ClassName "invalid" <$ guard (not loc.valid)))
        , HC.style
            case orientation of
              Orn.Horizontal → do
                C.top (C.px vect.y)
                C.left (C.px (vect.x + loc.offset))
                C.height (C.px vect.z)
              Orn.Vertical → do
                C.top (C.px (vect.y + loc.offset))
                C.left (C.px vect.x)
                C.width (C.px vect.z)
        , HE.onMouseMove (Just ∘ right ∘ H.action ∘ PreventDefault ∘ DOM.toEvent)
        ]
        [ renderGuideLabel loc.ratio
        , HH.div
            [ HP.classes [ HH.ClassName "sd-draftboard-split-edge-loc" ]
            , HC.style
                case orientation of
                  Orn.Horizontal →
                    C.width (C.px (Math.abs loc.offset))
                  Orn.Vertical →
                    C.height (C.px (Math.abs loc.offset))
            ]
            [ ]
        ]

    else
      HH.button
        [ HP.classes
            [ HH.ClassName "sd-draftboard-split-edge"
            , HH.ClassName (Orn.toString orientation)
            ]
        , HE.onMouseDown (HE.input \a _ → right (H.action (ResizeStart edge a)))
        , HC.style do
            C.top (C.px vect.y)
            C.left (C.px vect.x)
            case orientation of
              Orn.Horizontal → C.height (C.px vect.z)
              Orn.Vertical   → C.width (C.px vect.z)
        ]
        [ ]

unRational ∷ Rational → Ratio.Ratio Int
unRational (Rational.Rational a) = a
