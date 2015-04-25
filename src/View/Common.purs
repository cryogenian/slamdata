module View.Common where

import Control.Alternative (Alternative) 
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Config as Config
import qualified View.Css as Vc

navbar :: forall m p i. (Alternative m) => [H.HTML p (m i)] -> H.HTML p (m i)
navbar = H.nav [ A.classes [ B.navbar, B.navbarInverse, B.navbarFixedTop ] ]

row :: forall m p i. (Alternative m) => [H.HTML p (m i)] -> H.HTML p (m i)
row = H.div [ A.classes [ B.row ] ]

genericContainer :: forall m p i. (Alternative m) => [A.ClassName] -> [A.ClassName] ->
                    [H.HTML p (m i)] -> H.HTML p (m i) 
genericContainer wrapperClasses contentClasses nodes =
  H.div [ A.classes wrapperClasses ]
  [ row [ H.div [ A.classes contentClasses ]
          nodes
        ]
  ]

content :: forall m p i. (Alternative m) => [H.HTML p (m i)] -> H.HTML p (m i)
content = genericContainer
          [ B.container ]
          [ B.colMd8, B.colMdOffset2, B.colSm10, B.colSmOffset1 ]

contentFluid :: forall m p i. (Alternative m) => [H.HTML p (m i)] -> H.HTML p (m i)
contentFluid = genericContainer
               [ B.containerFluid ]
               [ ]


glyph :: forall p i. A.ClassName -> H.HTML p i
glyph g = H.i [ A.classes [B.glyphicon, g] ] []


icon :: forall p i. A.ClassName -> H.HTML p i
icon c = H.div [ A.classes [ B.colXs1, Vc.navIcon ] ]
             [ H.a [ A.href Config.homeHash
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
