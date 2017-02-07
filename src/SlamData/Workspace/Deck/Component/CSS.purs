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

module SlamData.Workspace.Deck.Component.CSS where

import Halogen.HTML.Core (ClassName(..))

deckContainer ∷ ClassName
deckContainer = ClassName "sd-deck-container"

deck ∷ ClassName
deck = ClassName "sd-deck"

deckFrame ∷ ClassName
deckFrame = ClassName "sd-deck-frame"

deckShadow ∷ ClassName
deckShadow = ClassName "sd-deck-shadow"

deckName ∷ ClassName
deckName = ClassName "sd-deck-name"

focused ∷ ClassName
focused = ClassName "sd-focused"

unfocused ∷ ClassName
unfocused = ClassName "sd-unfocused"

zoomDeck ∷ ClassName
zoomDeck = ClassName "sd-zoom-deck"

flipDeck ∷ ClassName
flipDeck = ClassName "sd-flip-deck"

grabDeck ∷ ClassName
grabDeck = ClassName "sd-grab-deck"

resizeDeck ∷ ClassName
resizeDeck = ClassName "sd-resize-deck"

dialogBackdrop ∷ ClassName
dialogBackdrop = ClassName "deck-dialog-backdrop"

-------------------------------------------------------------------------------

invisible ∷ ClassName
invisible = ClassName "sd-invisible"

-------------------------------------------------------------------------------

card ∷ ClassName
card = ClassName "sd-card"

cardSlider ∷ ClassName
cardSlider = ClassName "sd-card-slider"
