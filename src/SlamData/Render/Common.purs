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

module SlamData.Render.Common where

import SlamData.Prelude

import Data.Generic (gShow)

import Halogen as H
import Halogen.HTML.Core (HTML, ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Render.ClassName as CN
import SlamData.Render.Icon as I

row ∷ ∀ p f. Array (HTML p f) → HTML p f
row = HH.div [ HP.class_ $ H.ClassName "row" ]

content ∷ ∀ p f. Array (HTML p f) → HTML p f
content = HH.div [ HP.class_ CN.content ]

fadeWhen ∷ Boolean → Array ClassName
fadeWhen true = [ CN.fade ]
fadeWhen false = [ CN.fade, CN.in_ ]

classedDiv ∷ ∀ f p. ClassName → Array (HTML p (f Unit)) → HTML p (f Unit)
classedDiv cls = HH.div [ HP.classes [ cls ] ]

formGroup ∷ ∀ f p. Array (HTML p (f Unit)) → HTML p (f Unit)
formGroup = classedDiv CN.formGroup

clearFieldIcon ∷ ∀ f p. String → HTML p (f Unit)
clearFieldIcon label =
  HH.span
    [ HP.class_ (HH.ClassName "sd-clear-field-icon")
    , HP.title label
    , ARIA.label label
    ]
    [ I.removeSm
    , HH.span
        [ HP.class_ CN.srOnly ]
        [ HH.text label ]
    ]

busyFieldIcon ∷ ∀ f p. String → HTML p (f Unit)
busyFieldIcon label =
  HH.span
    [ HP.class_ (HH.ClassName "sd-busy-field-icon")
    , HP.title label
    , ARIA.label label
    ]
    [ HH.text label ]

svgElem ∷ ∀ r p i. HH.ElemName → Array (HP.IProp r i) → Array (HTML p i) → HTML p i
svgElem =
  HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg")

gripperDotsPattern ∷ ∀ p i. String → Int → Int → HTML p i
gripperDotsPattern i w h =
  let
    w' = gShow w
    h' = gShow h
    gdpid = "gripper-dots-pattern-" <> i
    attr' = HH.AttrName >>> HP.attr
  in
    svgElem (HH.ElemName "svg")
      [ attr' "width" w'
      , attr' "height" h'
      ]
      [ svgElem (HH.ElemName "defs") []
          [ svgElem (HH.ElemName "pattern")
              [ attr' "id" gdpid
              , attr' "height" "5"
              , attr' "width" "5"
              , attr' "x" "0"
              , attr' "y" "0"
              , attr' "patternUnits" "userSpaceOnUse"
              ]
              [ svgElem (HH.ElemName "circle")
                  [ attr' "cx" "2"
                  , attr' "cy" "2"
                  , attr' "r" "1.25"
                  , attr' "fill" "currentColor"
                  ] []
              ]
          ]
      , svgElem (HH.ElemName "rect")
          [ attr' "width" w'
          , attr' "height" h'
          , attr' "fill" $ "url(\"#" <> gdpid <> "\")"
          ] []
      ]
