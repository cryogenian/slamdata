module View.File.Breadcrumb (breadcrumbs) where

import Controller.File.Common (browseURL)
import Data.Maybe (Maybe(..))
import Model.File
import Model.File.Breadcrumb (Breadcrumb())
import View.File.Common (HTML())
import Optic.Core ((^.))

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.Themes.Bootstrap3 as B

breadcrumbs :: forall e. State -> HTML e
breadcrumbs state =
  H.ol [ A.classes [B.breadcrumb, B.colXs7] ]
       $ breadcrumb state <$> (state ^. _breadcrumbs)

breadcrumb :: forall e. State -> Breadcrumb -> HTML e
breadcrumb state b =
  H.li_ [ H.a [ A.href (browseURL Nothing
                                  (state ^. _sort)
                                  (state ^. _salt)
                                  b.link) ]
              [ H.text b.name ]
        ]
