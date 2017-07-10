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

module SlamData.Header.Attribution where


import SlamData.Prelude

import DOM.Event.Types (MouseEvent)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Render.Icon as I
import SlamData.Render.ClassName as CN

render ∷ ∀ p i . (MouseEvent → H.Action i) → H.Action i → H.HTML p i
render backdropDismiss dismiss =
  HH.div
    [ HP.classes [ HH.ClassName "sd-attributions", CN.dialogContainer ]
    , HE.onClick $ HE.input backdropDismiss
    ]
    [ HH.div
        [ HP.classes [ CN.dialog ] ]
        [ HH.h4_ [ HH.text "Attributions"]
        , HH.div
            [ HP.classes [ HH.ClassName "deck-dialog-body" ] ]
            [ attributions ]
        , HH.div
            [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
            [ HH.button
                [ HP.classes [ CN.btn, CN.btnDefault ]
                , HE.onClick (HE.input_ dismiss)
                ]
                [ HH.text "Done" ]
            ]
        ]
    ]

attributions ∷ ∀ p i. HH.HTML p i
attributions = HH.dl_ $ flip foldMap I.attributions \(title × names) →
  [ HH.dt_ [ HH.text title ] ] <> map (\n → HH.dd_ [ HH.text n ]) names
