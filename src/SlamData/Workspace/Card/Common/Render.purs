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

module SlamData.Workspace.Card.Common.Render where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

renderLowLOD
  ∷ ∀ f p
  . H.HTML p f
  → (CC.CardEvalQuery ~> f)
  → LevelOfDetails
  → H.HTML p f
renderLowLOD icon f = case _ of
  Low →
    HH.div
      [ HP.class_ (HH.className "card-input-minimum-lod") ]
      [ HH.button
          [ ARIA.label "Zoom or resize"
          , HP.title "Zoom or resize"
          , HE.onClick (HE.input_ (f ∘ CC.ZoomIn))
          ]
          [ icon
          , HH.text "Zoom or resize"
          ]
      ]
  _ →
    HH.text ""
