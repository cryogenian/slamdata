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

module SlamData.Workspace.Card.BuildChart.DimensionPicker.Component where

import SlamData.Prelude

import Data.Array as Array

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)

data Query s a
  = Choose Int a
  | Dismiss a
  | Confirm s a

type State s =
  { values ∷ Array s
  , selectedIndex ∷ Int
  }

picker
  ∷ ∀ s
  . String
  → (s → String)
  → H.Component (State s) (Query s) Slam
picker title showValue = H.component { render, eval }
  where
  render ∷ State s → H.ComponentHTML (Query s)
  render st =
    HH.div
      [ HP.classes [ HH.className "sd-dimension-picker" ] ]
      [ HH.div
          [ HP.classes [ HH.className "sd-dimension-picker-title" ] ]
          [ HH.h1_ [ HH.text title ]
          , HH.button
              [ HP.classes [ HH.className "sd-dismiss-button" ]
              , HP.title "Dismiss"
              , ARIA.label "Dismiss"
              , HE.onClick (HE.input_ Dismiss)
              ]
              [ HH.text "×"]
          ]
      , HH.div
          [ HP.classes [ HH.className "sd-dimension-picker-content" ] ]
          [ HH.ul_ (Array.mapWithIndex renderValue st.values) ]
      , HH.div
          [ HP.classes [ HH.className "sd-dimension-picker-toolbar" ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnDefault ]
              , ARIA.label "Dismiss"
              , HE.onClick (HE.input_ Dismiss)
              ]
              [ HH.text "Dismiss" ]
          , HH.button
              ([ HP.classes [ B.btn, B.btnPrimary ]
              , ARIA.label ""
              ] <>
                case Array.index st.values st.selectedIndex of
                  Just s  → [ HE.onClick (HE.input_ (Confirm s)) ]
                  Nothing → [ HP.disabled true ])
              [ HH.text "Confirm" ]
          ]
      ]
    where
    renderValue ix val =
      HH.li
        [ HP.classes (HH.className "selected" <$ guard (ix ≡ st.selectedIndex)) ]
        [ HH.button
            [ HP.title (showValue val)
            , ARIA.label (showValue val)
            , HE.onClick (HE.input_ (Choose ix))
            ]
            [ HH.text (showValue val) ]
        ]

  eval ∷ Query s ~> H.ComponentDSL (State s) (Query s) Slam
  eval = case _ of
    Choose ix next → do
      H.modify _ { selectedIndex = ix }
      pure next
    Dismiss next →
      pure next
    Confirm _ next →
      pure next
