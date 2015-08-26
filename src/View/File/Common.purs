{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

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
