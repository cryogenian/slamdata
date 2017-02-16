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
module SlamData.Guide.StepByStep.Component where

import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.Array as Array

import DOM (DOM)
import DOM.Classy.Event (toEvent)
import DOM.Event.Event (stopPropagation)
import DOM.Event.Types (Event)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Step =
  { imageUri ∷ String
  , text ∷ String
  }

type State =
  { steps ∷ Array Step
  , activeStep ∷ Int
  }

data Query a
  = StopPropagation Event a
  | Dismiss a
  | Next a
  | SetSteps (Array Step) a
  | SetActiveStep Int a

data Message = Dismissed

initialState ∷ Array Step → State
initialState steps =
  { steps
  , activeStep: 0
  }

component
  ∷ ∀ m eff
  . MonadEff (dom ∷ DOM | eff) m
  ⇒ H.Component HH.HTML Query (Array Step) Message m
component =
  H.component
    { render
    , eval
    , initialState
    , receiver: Just ∘ H.action ∘ SetSteps
    }

render ∷ State → H.ComponentHTML Query
render st =
  HH.div
    [ HE.onClick (HE.input_ Dismiss)
    , HP.class_ $ HH.ClassName "sd-step-by-step-guide-backdrop"
    ]
    [ HH.div
        [ HE.onClick (HE.input StopPropagation ∘ toEvent)
        , HP.class_ $ HH.ClassName "sd-step-by-step-guide"
        ]
        (Array.mapWithIndex renderStep st.steps <> buttons)
    ]
  where
  last ∷ Boolean
  last = Array.length st.steps - 1 ≡ st.activeStep

  renderStep ∷ Int → Step → H.ComponentHTML Query
  renderStep ix { imageUri, text } =
    HH.div
      [ HP.classes $
          [ HH.ClassName "sd-step-by-step-guide-step" ]
          <> (guard (ix ≡ st.activeStep) $> HH.ClassName "active")
      ]
      [ HH.img [ HP.src imageUri ]
      , HH.p_ [ HH.text text ]
      ]

  buttons ∷ Array (H.ComponentHTML Query)
  buttons =
    if last
      then
        [ HH.button
            [ HE.onClick (HE.input_ Dismiss) ]
            [ HH.text "Get started!" ]
        ]
      else
        [ HH.button
            [ HE.onClick (HE.input_ Next) ]
            [ HH.text "Next" ]
        , HH.button
            [ HE.onClick (HE.input_ Dismiss) ]
            [ HH.text "Skip" ]
        ]

eval
  ∷ ∀ m eff
  . MonadEff (dom ∷ DOM | eff) m
  ⇒ Query ~> H.ComponentDSL State Query Message m
eval = case _ of
  StopPropagation ev next →
    liftEff (stopPropagation ev) $> next
  Dismiss next →
    H.raise Dismissed $> next
  Next next → do
    H.modify \st → st { activeStep = st.activeStep + 1 }
    pure next
  SetActiveStep ix next → do
    st ← H.get
    case Array.length st.steps of
      0 → H.modify _ { activeStep = 0 }
      n → H.modify _ { activeStep = clamp 0 (n - 1) ix }
    pure next
  SetSteps steps next → do
    st ← H.get
    case Array.length steps of
      0 → H.put { steps, activeStep: 0 }
      n → H.put { steps, activeStep: clamp 0 (n - 1) st.activeStep }
    pure next
