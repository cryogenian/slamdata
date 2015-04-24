module View.File (view) where

import Controller.File (handleSetSort)
import Data.Tuple (Tuple(..))
import Model.File (State())
import Model.Sort (Sort(Asc, Desc), notSort)
import Utils.Halide (targetLink')
import View.File.Breadcrumb (breadcrumbs)
import View.File.Common (I(), glyph)
import View.File.Item (items)
import View.File.Modal (modal)
import View.File.Search (search)
import View.File.Toolbar (toolbar)
import qualified Data.StrMap as SM
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

view :: forall p e. State -> H.HTML p (I e)
view state =
  H.div_ [ navbar [ H.div [ A.classes [Vc.navCont, B.containerFluid] ]
                          [ icon, logo, search state ]
                  ]
         , content [ H.div [ A.class_ B.clearfix ]
                           [ breadcrumbs state.breadcrumbs
                           , toolbar state
                           ]
                   , row [ sorting state ]
                   , items state
                   ]
         , modal state
         ]

navbar :: forall p e. [H.HTML p (I e)] -> H.HTML p (I e)
navbar = H.nav [ A.classes [B.navbar, B.navbarInverse, B.navbarFixedTop] ]

icon :: forall p i. H.HTML p i
icon = H.div [ A.classes [ B.colXs1, Vc.navIcon ] ]
             [ H.a [ A.href Config.homeHash
                   , A.classes [B.navbarBrand, Vc.logo]
                   ]
                   [ glyph B.glyphiconFolderOpen ]
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

content :: forall p e. [H.HTML p (I e)] -> H.HTML p (I e)
content nodes = H.div [ A.class_ B.container ]
                      [ row [ H.div [ A.classes contentClasses ]
                                    nodes
                            ]
                      ]
  where
  contentClasses :: [A.ClassName]
  contentClasses = [B.colMd8, B.colMdOffset2, B.colSm10, B.colSmOffset1]

row :: forall p e. [H.HTML p (I e)] -> H.HTML p (I e)
row = H.div [ A.class_ B.row ]

sorting :: forall p e. State -> H.HTML p (I e)
sorting state =
  H.div [ A.classes [B.colXs4, Vc.toolbarSort] ]
        [ H.a (targetLink' $ handleSetSort $ notSort state.sort)
              [ H.text "Name"
              , H.i [ chevron state
                    , A.style (A.styles $ SM.fromList [Tuple "margin-left" "10px"])
                    ]
                    []
              ]
        ]
  where
  chevron { sort: Asc  } = A.classes [B.glyphicon, B.glyphiconChevronUp]
  chevron { sort: Desc } = A.classes [B.glyphicon, B.glyphiconChevronDown]
