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

module Utils.DOM where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff())
import Data.Maybe (Maybe())
import Data.Nullable (toMaybe)
import DOM (DOM())
import DOM.HTML.Types (HTMLElement(), htmlElementToElement)
import DOM.Node.Types (elementToParentNode, Element())
import DOM.Node.ParentNode as P
import Unsafe.Coerce (unsafeCoerce)

elementToHTMLElement :: Element -> HTMLElement
elementToHTMLElement = unsafeCoerce

querySelector :: forall e. String -> HTMLElement ->
                 Eff (dom :: DOM|e) (Maybe HTMLElement)
querySelector str htmlEl =
  map (toMaybe >>> map elementToHTMLElement)
  $ P.querySelector str $ elementToParentNode $ htmlElementToElement htmlEl

foreign import waitLoaded :: forall e. Aff (dom :: DOM |e) Unit
foreign import onLoad :: forall e. Eff e Unit -> Eff e Unit
