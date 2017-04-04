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

import Halogen.HTML.Core (HTML, ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Render.CSS as Rc

row ∷ ∀ p f. Array (HTML p f) → HTML p f
row = HH.div [ HP.class_ B.row ]

content ∷ ∀ p f. Array (HTML p f) → HTML p f
content = HH.div [ HP.class_ Rc.content ]

fadeWhen ∷ Boolean → Array ClassName
fadeWhen true = [ B.fade ]
fadeWhen false = [ B.fade, B.in_ ]

classedDiv ∷ ∀ f p. ClassName → Array (HTML p (f Unit)) → HTML p (f Unit)
classedDiv cls = HH.div [ HP.classes [ cls ] ]

formGroup ∷ ∀ f p. Array (HTML p (f Unit)) → HTML p (f Unit)
formGroup = classedDiv B.formGroup

searchFieldIcon ∷ ∀ f p. HTML p (f Unit)
searchFieldIcon =
  HH.span
    [ HP.class_ (HH.ClassName "sd-search-field-icon") ]
    []

clearFieldIcon ∷ ∀ f p. String → HTML p (f Unit)
clearFieldIcon label =
  HH.span
    [ HP.class_ (HH.ClassName "sd-clear-field-icon")
    , HP.title label
    , ARIA.label label
    ]
    [ HH.text label ]

busyFieldIcon ∷ ∀ f p. String → HTML p (f Unit)
busyFieldIcon label =
  HH.span
    [ HP.class_ (HH.ClassName "sd-busy-field-icon")
    , HP.title label
    , ARIA.label label
    ]
    [ HH.text label ]
