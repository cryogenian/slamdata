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

module View.Common where

import Prelude
import Data.Array (singleton)
import Data.Char (fromCharCode)
import Data.Maybe (Maybe(), maybe)
import Data.String (fromChar)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Types as ET
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Config as Config
import qualified View.Css as Vc

navbar :: forall i. Array (H.HTML i) -> H.HTML i
navbar = H.nav [ A.class_ B.clearfix ]

row :: forall i. Array (H.HTML i) -> H.HTML i
row = H.div [ A.class_ B.row ]

row' :: forall i. Array (A.ClassName) -> Array (H.HTML i) -> H.HTML i
row' cs = H.div [ A.classes $ cs ++ [B.row] ]

genericContainer :: forall i. Array (A.ClassName) -> Array (A.ClassName) ->
                    Array (H.HTML i) -> H.HTML i
genericContainer wrapperClasses contentClasses nodes =
  H.div [ A.classes wrapperClasses ]
  [ row [ H.div [ A.classes contentClasses ]
          nodes
        ]
  ]

content :: forall i. Array (H.HTML i) -> H.HTML i
content = H.div [ A.class_ Vc.content ]

contentFluid :: forall i. Array (H.HTML i) -> H.HTML i
contentFluid = genericContainer
               [ B.containerFluid ]
               [ ]


glyph :: forall i. A.ClassName -> H.HTML i
glyph g = H.i [ A.classes [B.glyphicon, g] ] []


icon :: forall i. A.ClassName -> String -> H.HTML i
icon c href = H.div [ A.classes [Vc.navIcon] ]
             [ H.a [ A.href href ]
                   [ glyph c ]
             ]

logo :: forall i. Maybe String -> H.HTML i
logo mbVersion =
  H.div [ A.class_ Vc.navLogo ]
  [ H.a ([ A.href Config.slamDataHome ] <> title mbVersion)
    [ H.img [A.src "img/logo.svg"] [] ]
  ]
  where
  title = maybe [] (singleton <<< A.title <<< ("Version " <>))

closeButton :: forall i. (ET.Event ET.MouseEvent -> E.EventHandler i) -> H.HTML i
closeButton handler =
  H.button [ A.class_ B.close
           , E.onClick handler ]
           [ H.span_ [ H.text (fromChar $ fromCharCode 215) ] ]

fadeWhen :: Boolean -> Array (A.ClassName)
fadeWhen true = [B.fade]
fadeWhen false = [B.fade, B.in_]
