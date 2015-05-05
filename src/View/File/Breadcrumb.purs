module View.File.Breadcrumb (breadcrumbs) where

import Controller.File (breadcrumbClicked)
import EffectTypes (FileAppEff())
import Model.Breadcrumb (Breadcrumb())
import Utils.Halide (targetLink')
import View.File.Common (I())
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

breadcrumbs :: forall e. [Breadcrumb] -> H.HTML (I e)
breadcrumbs breadcrumbs =
  H.ol [ A.classes [B.breadcrumb, B.colXs8] ]
       (breadcrumb <$> breadcrumbs)

breadcrumb :: forall e. Breadcrumb -> H.HTML (I e)
breadcrumb b = H.li_ [ H.a (targetLink' $ breadcrumbClicked b)
                           [ H.text b.name ]
                     ]
