{-
Copyright 2017 SlamData, Inc.

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
module SlamData.Hint where

import SlamData.Prelude
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Render.Icon as I

data Arrow
  = RightArrow
  | LeftArrow
  | UpArrow
  | DownArrow

arrowClassName ∷ Arrow → HH.ClassName
arrowClassName = case _ of
  RightArrow → HH.ClassName "sd-hint-right-arrow"
  LeftArrow → HH.ClassName "sd-hint-left-arrow"
  UpArrow → HH.ClassName "sd-hint-up-arrow"
  DownArrow → HH.ClassName "sd-hint-down-arrow"

render ∷ ∀ f a. Arrow → HH.ClassName → Maybe (Unit → f Unit) → String → HH.HTML a (f Unit)
render arrow className dismissQuery text =
  HH.div
    [ HP.classes [ HH.ClassName "sd-hint", className ] ]
    [ HH.div
        [ HP.classes
            [ HH.ClassName "sd-notification"
            , arrowClassName arrow
            ]
        ]
        [ HH.div
            [ HP.class_ $ HH.ClassName "sd-notification-text" ]
            [ HH.text text ]
        , maybe (HH.text "") renderDismissButton dismissQuery
        ]
    ]

renderDismissButton ∷ ∀ f a. (Unit → f Unit) → HH.HTML a (f Unit)
renderDismissButton dismissQuery =
  HH.div
    [ HP.class_ $ HH.ClassName "sd-notification-buttons" ]
    [ HH.button
        [ HP.classes [ HH.ClassName "sd-notification-dismiss" ]
        , HE.onClick (HE.input_ dismissQuery)
        , ARIA.label "Dismiss"
        ]
        [ I.closeSm ]
    ]
