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

import Data.Char (fromCharCode)
import Data.String (singleton)

import Halogen.HTML.Core (HTML, ClassName)
import Halogen.HTML.Events.Handler (EventHandler)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Types as ET
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Config as Config
import SlamData.Render.CSS as Rc

navbar ∷ ∀ p f. Array (HTML p f) → HTML p f
navbar = H.nav []

row ∷ ∀ p f. Array (HTML p f) → HTML p f
row = H.div [ P.class_ B.row ]

row' ∷ ∀ p f. Array ClassName → Array (HTML p f) → HTML p f
row' cs = H.div [ P.classes $ cs <> [ B.row ] ]

genericContainer ∷ ∀ p f. Array ClassName → Array ClassName →
                    Array (HTML p f) → HTML p f
genericContainer wrapperClasses contentClasses nodes =
  H.div [ P.classes wrapperClasses ]
  [ row [ H.div [ P.classes contentClasses ]
          nodes
        ]
  ]

content ∷ ∀ p f. Array (HTML p f) → HTML p f
content = H.div [ P.class_ Rc.content ]

contentFluid ∷ ∀ p f. Array (HTML p f) → HTML p f
contentFluid = genericContainer [ B.containerFluid ] [ ]

glyph ∷ ∀ p f. ClassName → HTML p f
glyph g = H.i [ P.classes [ B.glyphicon, g ] ] [ ]

glyphInactive ∷ ∀ p f. ClassName → HTML p f
glyphInactive g = H.i [ P.classes [B.glyphicon, Rc.glyphiconInactive, g ] ] []

icon ∷ ∀ p f. ClassName → String → String → HTML p f
icon c href label =
  H.div
    [ P.classes [ Rc.navIcon ], P.title label, ARIA.label label ]
    [ H.a
        [ P.href href ]
        [ glyph c ]
    ]

icon' ∷ ∀ p f. ClassName → String → String → HTML p f
icon' c title href = H.div [ P.classes [ Rc.navIcon ] ]
                     [ H.a [ P.href href
                           , P.title title
                           , ARIA.label title
                           ]
                       [ glyph c ]
                     ]

logo ∷ ∀ p f. Maybe String → HTML p f
logo mbVersion =
  H.div [ P.class_ Rc.navLogo ]
  ([ H.a
     [ P.href Config.slamDataHome
     , ARIA.label "Browse root folder"
     , P.title "Browse root folder"
     ]
     [ H.img [ P.src "img/logo.svg" ] ]
   ]
   ⊕ foldMap (pure ∘ H.div_ ∘ pure ∘ H.text) mbVersion
  )

closeButton ∷ ∀ p f. (ET.Event ET.MouseEvent → EventHandler f) → HTML p f
closeButton handler =
  H.button [ P.class_ B.close
           , E.onClick (map Just <$> handler)
           ]
  [ H.span_ [ H.text (singleton $ fromCharCode 215) ] ]

fadeWhen ∷ Boolean → Array ClassName
fadeWhen true = [ B.fade ]
fadeWhen false = [ B.fade, B.in_ ]

classedDiv ∷ ∀ f p. ClassName → Array (HTML p (f Unit)) → HTML p (f Unit)
classedDiv cls = H.div [ P.classes [ cls ] ]

formGroup ∷ ∀ f p. Array (HTML p (f Unit)) → HTML p (f Unit)
formGroup = classedDiv B.formGroup
