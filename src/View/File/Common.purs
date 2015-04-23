module View.File.Common where

import Control.Alternative (Alternative)
import EffectTypes (FileAppEff ())
import Model.File (Input())
import Utils.Halide (targetLink')
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

type I e = E.Event (FileAppEff e) Input

glyph :: forall p i. A.ClassName -> H.HTML p i
glyph g = H.i [ A.classes [B.glyphicon, g] ] []

toolItem :: forall a m i p. (Alternative m) => [A.ClassName] -> a -> (a -> m i) -> String -> A.ClassName -> H.HTML p (m i)
toolItem classes actionArg action title icon =
  H.li_ [ H.a (targetLink' $ action actionArg)
              [ H.i [ A.title title
                    , A.classes (classes ++ [B.glyphicon, icon])
                    ]
                    []
              ]
        ]
