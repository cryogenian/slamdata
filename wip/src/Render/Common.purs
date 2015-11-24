{-
Copyright 2015 SlamData, Inc.

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

module Render.Common where

import Prelude

import Data.Array (singleton)
import Data.Char (fromCharCode)
import Data.Maybe (Maybe(), maybe)
import Data.String (fromChar)

import Halogen.HTML as H
import Halogen.HTML.Core (HTML(), ClassName())
import Halogen.HTML.Events as E
import Halogen.HTML.Events.Types as ET
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B

import Config as Config
import Render.CssClasses as Rc

navbar :: forall p f. Array (HTML p f) -> HTML p f
navbar = H.nav [ P.class_ B.clearfix ]

row :: forall p f. Array (HTML p f) -> HTML p f
row = H.div [ P.class_ B.row ]

row' :: forall p f. Array ClassName -> Array (HTML p f) -> HTML p f
row' cs = H.div [ P.classes $ cs <> [ B.row ] ]

genericContainer :: forall p f. Array ClassName -> Array ClassName ->
                    Array (HTML p f) -> HTML p f
genericContainer wrapperClasses contentClasses nodes =
  H.div [ P.classes wrapperClasses ]
  [ row [ H.div [ P.classes contentClasses ]
          nodes
        ]
  ]

content :: forall p f. Array (HTML p f) -> HTML p f
content = H.div [ P.class_ Rc.content ]

contentFluid :: forall p f. Array (HTML p f) -> HTML p f
contentFluid = genericContainer [ B.containerFluid ] [ ]

glyph :: forall p f. ClassName -> HTML p f
glyph g = H.i [ P.classes [ B.glyphicon, g ] ] [ ]

glyphInactive :: forall p f. ClassName -> HTML p f
glyphInactive g = H.i [ P.classes [B.glyphicon, Rc.glyphiconInactive, g ] ] []

icon :: forall p f. ClassName -> String -> HTML p f
icon c href = H.div [ P.classes [ Rc.navIcon ] ]
              [ H.a [ P.href href ]
                [ glyph c ]
              ]

logo :: forall p f. Maybe String -> HTML p f
logo mbVersion =
  H.div [ P.class_ Rc.navLogo ]
  [ H.a ( [ P.href Config.slamDataHome ] <> title mbVersion )
    [ H.img [ P.src "img/logo.svg" ] ]
  ]
  where
  title = maybe [ ] (singleton <<< P.title <<< (append "Version "))

closeButton :: forall p f. (ET.Event ET.MouseEvent -> E.EventHandler f) -> HTML p f
closeButton handler =
  H.button [ P.class_ B.close
           , E.onClick handler
           ]
  [ H.span_ [ H.text (fromChar $ fromCharCode 215) ] ]

fadeWhen :: Boolean -> Array ClassName
fadeWhen true = [ B.fade ]
fadeWhen false = [ B.fade, B.in_ ]

classedDiv :: forall f p. ClassName -> Array (HTML p (f Unit)) -> HTML p (f Unit)
classedDiv cls = H.div [ P.classes [ cls ] ]

formGroup :: forall f p. Array (HTML p (f Unit)) -> HTML p (f Unit)
formGroup = classedDiv B.formGroup
