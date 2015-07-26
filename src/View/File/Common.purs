module View.File.Common where

import Prelude
import Control.Alternative (Alternative)
import Controller.File.Common (Event())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

type HTML e = H.HTML (Event e)

toolItem :: forall a m i. (Alternative m) => Array (A.ClassName) -> a -> (a -> m i) -> String -> A.ClassName -> H.HTML (m i)
toolItem classes actionArg action title icon =
  H.li_ [ H.button [ E.onClick (\_ -> pure $ action actionArg) ]
                   [ H.i [ A.title title
                         , A.classes (classes ++ [B.glyphicon, icon])
                         ]
                         []
                   ]
        ]
