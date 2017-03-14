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

module SlamData.Workspace.Card.Setups.Dialog where

import SlamData.Prelude


import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

type DialogOptions s p i =
  { onDismiss ∷ H.Action i
  , onConfirm ∷ s → H.Action i
  , classes ∷ Array HH.ClassName
  , selection ∷ Maybe s
  , isSelectable ∷ s → Boolean
  , title ∷ Array (H.HTML p i)
  , content ∷ Array (H.HTML p i)
  }

pickerDialog ∷ forall s p i. DialogOptions s p i → H.HTML p i
pickerDialog opts =
  HH.div
    [ HP.classes ([ HH.ClassName "sd-picker-dialog" ] <> opts.classes) ]
    [ HH.div
        [ HP.classes [ HH.ClassName "sd-picker-dialog-title" ] ]
        [ HH.h1_ opts.title
        , HH.button
            [ HP.classes [ HH.ClassName "sd-dismiss-button" ]
            , HP.title "Dismiss"
            , ARIA.label "Dismiss"
            , HE.onClick $ HE.input_ opts.onDismiss
            ]
            [ HH.text "×"]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "sd-picker-dialog-content" ] ]
        opts.content
    , HH.div
        [ HP.classes [ HH.ClassName "sd-picker-dialog-toolbar" ] ]
        [ HH.button
            [ HP.classes [ B.btn, B.btnDefault ]
            , ARIA.label "Dismiss"
            , HE.onClick $ HE.input_ opts.onDismiss
            ]
            [ HH.text "Dismiss" ]
        , HH.button
            ([ HP.classes [ B.btn, B.btnPrimary ]
            , ARIA.label ""
            ] <>
              case opts.selection of
                Just sel | opts.isSelectable sel →
                  [ HE.onClick $ HE.input_ $ opts.onConfirm sel
                  , HP.disabled false
                  ]
                _ →
                  [ HP.disabled true ])
            [ HH.text "Confirm" ]
        ]
    ]
