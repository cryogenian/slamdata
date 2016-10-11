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

import Halogen.HTML.Core (className, ClassName)

deckContainer ∷ ClassName
deckContainer = className "sd-deck-container"

deck ∷ ClassName
deck = className "sd-deck"

deckFrame ∷ ClassName
deckFrame = className "sd-deck-frame"

deckShadow ∷ ClassName
deckShadow = className "sd-deck-shadow"

deckName ∷ ClassName
deckName = className "sd-deck-name"

focused ∷ ClassName
focused = className "sd-focused"

unfocused ∷ ClassName
unfocused = className "sd-unfocused"

zoomDeck ∷ ClassName
zoomDeck = className "sd-zoom-deck"

flipDeck ∷ ClassName
flipDeck = className "sd-flip-deck"

grabDeck ∷ ClassName
grabDeck = className "sd-grab-deck"

resizeDeck ∷ ClassName
resizeDeck = className "sd-resize-deck"

dialogWrapper ∷ ClassName
dialogWrapper = className "deck-dialog-wrapper"

-------------------------------------------------------------------------------

invisible ∷ ClassName
invisible = className "sd-invisible"

-------------------------------------------------------------------------------

card ∷ ClassName
card = className "sd-card"

cardSlider ∷ ClassName
cardSlider = className "sd-card-slider"
