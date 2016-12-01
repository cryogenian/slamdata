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

module SlamData.Workspace.Deck.Component.Render
  ( renderDeck
  , renderError
  , renderSignInButton
  ) where

import SlamData.Prelude

import Data.List as L

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Quasar.Error as QE
import SlamData.Render.Common (glyph)
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Common (DeckOptions, DeckHTML)
import SlamData.Workspace.Deck.Component.CSS as CSS
import SlamData.Workspace.Deck.Component.ChildSlot (cpIndicator, cpBackSide, cpDialog)
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (Query(..))
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Indicator.Component as Indicator
import SlamData.Workspace.Deck.Slider as Slider

import Quasar.Advanced.Types (ProviderR)

renderError ∷ ∀ f a. QE.QError → HH.HTML a (f Unit)
renderError err =
  HH.div
    [ HP.classes [ HH.className "sd-workspace-error" ] ]
    [ HH.h1_
        [ HH.text "Couldn't load this SlamData deck." ]
    , HH.p_
        [ HH.text $ QE.printQError err ]
    ]

renderSignInButton ∷ ProviderR → DeckHTML
renderSignInButton providerR =
    HH.button
      [ HE.onClick $ HE.input_ $ SignIn providerR
      , HP.classes [ HH.className "btn", HH.className "btn-primary" ]
      , HP.buttonType HP.ButtonButton
      ]
      [ HH.text $ "Sign in with " ⊕ providerR.displayName ]

renderDeck ∷ DeckOptions → (DeckOptions → DeckComponent) → DCS.State → DeckHTML
renderDeck opts deckComponent st =
  HH.div
    (deckClasses st
     ⊕ deckProperties opts
     ⊕ Slider.containerProperties st)
    [ HH.div
        [ HP.class_ CSS.deckFrame
        , HE.onMouseDown \ev →
            if st.focused && (not (L.null opts.cursor))
              then HEH.stopPropagation *> pure (Just (Defocus ev unit))
              else pure Nothing
        ]
        $ frameElements opts ⊕ [ renderName st.name ]
    , HH.div
        [ HP.class_ CSS.deck
        , HP.key "deck"
        ]
        [ Slider.render opts deckComponent st $ st.displayMode ≡ DCS.Normal
        , renderBackside $ st.displayMode ≡ DCS.Backside
        , renderDialog $ st.displayMode ≡ DCS.Dialog
        ]
    ]

  where

  renderDialog visible =
    HH.div
      [ HP.classes $ [CSS.dialogWrapper] ⊕ (guard (not visible) $> CSS.invisible)
      , ARIA.hidden $ show $ not visible
      ]
      [ dialogSlot ]

  renderBackside visible =
    HH.div
      [ HP.classes $ [CSS.cardSlider] ⊕ (guard (not visible) $> CSS.invisible)
      , ARIA.hidden $ show $ not visible
      ]
      [ backside ]

deckClasses ∷ ∀ r. DCS.State → Array (HP.IProp (HP.InteractiveEvents (HP.GlobalProperties r)) (Query Unit))
deckClasses st =
  pure $ HP.classes $
    [ CSS.deckContainer
    , HH.className (responsiveSize st.responsiveSize)
    , if st.focused then CSS.focused else CSS.unfocused
    ]

deckProperties ∷ ∀ r. DeckOptions → Array (HP.IProp (HP.InteractiveEvents (HP.GlobalProperties r)) (Query Unit))
deckProperties opts =
  [ HP.key "deck-container"
  , HP.ref (H.action ∘ SetCardElement)
  ] ⊕ (guard (L.length opts.cursor <= 1) $>
        HE.onMouseDown \_ → HEH.stopPropagation $> Just (H.action Focus))

renderName ∷ String → DeckHTML
renderName name =
  HH.div
    [ HP.class_ CSS.deckName ]
    [ HH.text name ]

frameElements ∷ DeckOptions → Array DeckHTML
frameElements { accessType, cursor }
  | accessType ≡ AT.ReadOnly = mempty
  | L.null cursor = rootFrameElements
  | otherwise = childFrameElements

rootFrameElements ∷ Array DeckHTML
rootFrameElements =
  [ zoomOutButton
  , flipButton
  , deckIndicator
  ]

childFrameElements ∷ Array DeckHTML
childFrameElements =
  [ zoomInButton
  , flipButton
  , moveGripper
  , deckIndicator
  ]

dialogSlot ∷ DeckHTML
dialogSlot =
  HH.slot' cpDialog unit \_ →
    { component: Dialog.comp
    , initialState: H.parentState Dialog.initialState
    }

backside ∷ DeckHTML
backside =
  HH.div
    [ HP.classes [ CSS.card ] ]
    [ HH.slot' cpBackSide unit \_ →
        { component: Back.comp
        , initialState: Back.initialState
        }
    ]

deckIndicator ∷ DeckHTML
deckIndicator =
  HH.slot' cpIndicator unit \_ →
    { component: Indicator.comp
    , initialState: Indicator.initialState
    }

flipButton ∷ DeckHTML
flipButton =
  HH.button
    [ HP.classes [ CSS.flipDeck ]
    , HE.onClick (HE.input_ FlipDeck)
    , ARIA.label "Flip deck"
    , HP.title "Flip deck"
    ]
    []

moveGripper ∷ DeckHTML
moveGripper =
  HH.button
    [ HP.classes [ CSS.grabDeck ]
    , HE.onMouseDown (HE.input GrabDeck)
    , ARIA.label "Grab deck"
    , HP.title "Grab deck"
    ]
    []

zoomInButton ∷ DeckHTML
zoomInButton =
  HH.button
     [ HP.classes [ CSS.zoomDeck ]
     , ARIA.label "Zoom in"
     , HP.title "Zoom in"
     , HE.onClick (HE.input_ ZoomIn)
     ]
     [ glyph B.glyphiconZoomIn ]

zoomOutButton ∷ DeckHTML
zoomOutButton =
  HH.button
   [ HP.classes [ CSS.zoomDeck ]
   , ARIA.label "Zoom out"
   , HP.title "Zoom out"
   , HE.onClick (HE.input_ ZoomOut)
   ]
   [ glyph B.glyphiconZoomOut ]

responsiveSize ∷ DCS.ResponsiveSize → String
responsiveSize = case _ of
  DCS.XSmall  → "sd-size-xsmall"
  DCS.Small   → "sd-size-small"
  DCS.Medium  → "sd-size-medium"
  DCS.Large   → "sd-size-large"
  DCS.XLarge  → "sd-size-xlarge"
  DCS.XXLarge → "sd-size-xxlarge"
