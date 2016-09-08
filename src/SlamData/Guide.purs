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
module SlamData.Guide where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

data Arrow = RightArrow | DownArrow

arrowClassName ∷ Arrow → HH.ClassName
arrowClassName =
  case _ of
    RightArrow -> HH.className "sd-guide-right-arrow"
    DownArrow -> HH.className "sd-guide-down-arrow"

render ∷ ∀ f a. Arrow → HH.ClassName → (Unit → f Unit) → String → HH.HTML a (f Unit)
render arrow className dismissQuery text =
  HH.div
    [ HP.classes [ HH.className "sd-guide", className ] ]
    [ HH.div
        [ HP.classes
            [ HH.className "sd-notification"
            , arrowClassName arrow
            ]
        ]
        [ HH.div
            [ HP.class_ $ HH.className "sd-notification-text" ]
            [ HH.text text ]
        , HH.div
            [ HP.class_ $ HH.className "sd-notification-buttons" ]
            [ HH.button
                [ HP.classes [ HH.className "sd-notification-dismiss" ]
                , HE.onClick (HE.input_ dismissQuery)
                , ARIA.label "Dismiss"
                ]
                [ HH.text "×" ]
            ]
        ]
    ]

renderStepByStep
  ∷ ∀ f a
  . { next ∷ Unit → f Unit, dismiss ∷ Unit → f Unit }
  → String
  → String
  → Boolean
  → HH.HTML a (f Unit)
renderStepByStep queries imageUri text last =
  HH.div
    [ HE.onClick \_ → EH.stopPropagation $> Just (H.action $ queries.dismiss)
    , HP.class_ $ HH.className "sd-step-by-step-guide-backdrop"
    ]
    [ HH.div
      [ HP.class_ $ HH.className "sd-step-by-step-guide"
      , HE.onClick (\_ → EH.stopPropagation $> Nothing)
      ]
      ([ HH.img [ HP.src imageUri ] , HH.p_ [ HH.text text ] ] <> pure buttons)
    ]
      where
      buttons =
        HH.div_
          $ if last
            then
              [ HH.button
                  [ HE.onClick \_ → EH.stopPropagation $> Just (H.action queries.dismiss) ]
                  [ HH.text "Get started!" ]
              ]
            else
              [ HH.button
                 [ HE.onClick \_ → EH.stopPropagation $> Just (H.action queries.next) ]
                 [ HH.text "Next" ]
              , HH.button
                 [ HE.onClick \_ → EH.stopPropagation $> Just (H.action queries.dismiss) ]
                  [ HH.text "Skip" ]
              ]
