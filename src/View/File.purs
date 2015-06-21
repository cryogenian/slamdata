module View.File (view) where

import Controller.File.Common (browseURL)
import Css.Geometry
import Css.Size
import Css.String
import Data.These (theseRight)
import Data.Tuple (Tuple(..))
import Model.File
import Model.File.Search (_value)
import Model.File.Sort (Sort(..), notSort)
import Optic.Core ((..), (^.))
import View.Common (glyph)
import View.Common (navbar, icon, logo, content, row)
import View.File.Breadcrumb (breadcrumbs)
import View.File.Common (HTML())
import View.File.Item (items)
import View.File.Modal (modal)
import View.File.Search (search)
import View.File.Toolbar (toolbar)

import qualified Data.StrMap as SM
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.CSS as CSS
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Utils as U
import qualified View.Css as Vc

view :: forall e. State -> HTML e
view state =
  H.div_ [ navbar [ H.div [ A.classes [Vc.header, B.clearfix] ]
                          [ icon B.glyphiconFolderOpen Config.homeHash
                          , logo
                          , search state ]
                  ]
         , content [ H.div [ A.class_ B.clearfix ]
                           [ breadcrumbs state
                           , toolbar state
                           ]
                   , row [ sorting state ]
                   , items state
                   ]
         , modal state
         ]

sorting :: forall e. State -> HTML e
sorting state =
  H.div [ A.classes [B.colXs4, Vc.toolbarSort] ]
        [ H.a [ A.href (browseURL (theseRight $ state ^. _search .. _value) (notSort (state ^. _sort)) (state ^. _salt) (state ^. _path)) ]
              [ H.text "Name"
              , H.i [ chevron (state ^. _sort)
                    , CSS.style (marginLeft $ px 10)
                    ]
                    []
              ]
        ]
  where
  chevron Asc = A.classes [B.glyphicon, B.glyphiconChevronUp]
  chevron Desc = A.classes [B.glyphicon, B.glyphiconChevronDown]
