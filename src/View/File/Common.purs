module View.File.Common where

import Control.Alternative (Alternative)
import Input.File (Input())
import EffectTypes (FileAppEff ())
import Utils.Halide (targetLink')
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

type I e = E.Event (FileAppEff e) Input

toolItem :: forall a m i. (Alternative m) => [A.ClassName] -> a -> (a -> m i) -> String -> A.ClassName -> H.HTML (m i)
toolItem classes actionArg action title icon =
  H.li_ [ H.button [ E.onClick (\_ -> pure $ action actionArg) ]
                   [ H.i [ A.title title
                         , A.classes (classes ++ [B.glyphicon, icon])
                         ]
                         []
                   ]
        ]
