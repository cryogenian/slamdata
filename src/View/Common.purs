module View.Common where

import Data.Char (fromCharCode)
import Data.String (fromChar)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Types as ET
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Config as Config
import qualified View.Css as Vc

navbar :: forall p i. [H.HTML p i] -> H.HTML p i
navbar = H.nav [ A.classes [ B.navbar, B.navbarInverse, B.navbarFixedTop ] ]

row :: forall p i. [H.HTML p i ] -> H.HTML p i
row = H.div [ A.classes [ B.row ] ]

genericContainer :: forall p i. [A.ClassName] -> [A.ClassName] ->
                    [H.HTML p i] -> H.HTML p i
genericContainer wrapperClasses contentClasses nodes =
  H.div [ A.classes wrapperClasses ]
  [ row [ H.div [ A.classes contentClasses ]
          nodes
        ]
  ]

content :: forall p i. [H.HTML p i] -> H.HTML p i
content = genericContainer
          [ B.container ]
          [ B.colMd8, B.colMdOffset2, B.colSm10, B.colSmOffset1 ]

contentFluid :: forall p i. [H.HTML p i] -> H.HTML p i
contentFluid = genericContainer
               [ B.containerFluid ]
               [ ]


glyph :: forall p i. A.ClassName -> H.HTML p i
glyph g = H.i [ A.classes [B.glyphicon, g] ] []


icon :: forall p i. A.ClassName -> String -> H.HTML p i
icon c href = H.div [ A.classes [ B.colXs1, Vc.navIcon ] ]
             [ H.a [ A.href href
                  --A.href Config.homeHash
                   , A.classes [B.navbarBrand, Vc.logo]
                   ]
                   [ glyph c ]
             ]

logo :: forall p i. H.HTML p i
logo = H.div [ A.classes [ B.colXs3, Vc.navLogo ] ]
             [ H.a [ A.href Config.slamDataHome
                   , A.classes [B.navbarBrand, Vc.logo]
                   ]
                   [ H.img [A.src "img/logo.svg"]
                           []
                   ]
             ]

closeButton :: forall p i. (ET.Event ET.MouseEvent -> E.EventHandler i) -> H.HTML p i
closeButton handler =
  H.button [ A.class_ B.close
           , E.onClick handler ]
           [ H.span_ [ H.text (fromChar $ fromCharCode 215) ] ]
