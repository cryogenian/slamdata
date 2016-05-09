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

module SlamData.Render.CSS.New where

import Halogen.HTML.Core (className, ClassName)

pagination ∷ ClassName
pagination = className "sd-pagination"

deckBackSide ∷ ClassName
deckBackSide = className "sd-deck-backside"

loading ∷ ClassName
loading = className "loading"

cardSlider :: ClassName
cardSlider = className "sd-card-slider"

board :: ClassName
board = className "sd-board"

deck :: ClassName
deck = className "sd-deck"

card :: ClassName
card = className "sd-card"

cardGripper :: ClassName
cardGripper = className "sd-card-gripper"

cardGripperLast :: ClassName
cardGripperLast = className "sd-card-gripper-last"

flipDeck :: ClassName
flipDeck = className "sd-flip-deck"

form ∷ ClassName
form = className "sd-form"

formButton ∷ ClassName
formButton = className "sd-form-button"

formButtonGroup ∷ ClassName
formButtonGroup = className "sd-form-button-group"
