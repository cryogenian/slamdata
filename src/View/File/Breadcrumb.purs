module View.File.Breadcrumb where

import Controller.File (breadcrumbClicked)
import EffectTypes
import Model.Breadcrumb
import Model.File (Input())
import Utils.Halide (targetLink')
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

breadcrumbs :: forall p e. [Breadcrumb] -> H.HTML p (E.Event (FileAppEff e) Input)
breadcrumbs breadcrumbs =
  H.ol [ A.classes [B.breadcrumb, B.colXs8] ]
       (breadcrumbItem <$> breadcrumbs)
  where
  breadcrumbItem :: Breadcrumb -> H.HTML p (E.Event (FileAppEff e) Input)
  breadcrumbItem b = H.li_ [ H.a (targetLink' $ breadcrumbClicked b)
                                 [ H.text b.name ]
                           ]
