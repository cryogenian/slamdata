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
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Bind ((=<<))
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement(), htmlElementToElement, htmlDocumentToDocument)
import DOM.HTML.Window (document, navigator)
import DOM.HTML.Navigator (platform)
import DOM.Node.ParentNode as P
import DOM.Node.Types (elementToParentNode, Element(), documentToEventTarget)
import Data.Maybe (Maybe())
import Data.Nullable (toMaybe)
import Data.String (take)
import Unsafe.Coerce (unsafeCoerce)

elementToHTMLElement :: Element -> HTMLElement
elementToHTMLElement = unsafeCoerce

querySelector :: forall e. String -> HTMLElement ->
                 Eff (dom :: DOM|e) (Maybe HTMLElement)
querySelector str htmlEl =
  map (toMaybe >>> map elementToHTMLElement)
  $ P.querySelector str $ elementToParentNode $ htmlElementToElement htmlEl

documentTarget :: _
documentTarget = htmlDocumentToEventTarget <$> (document =<< window)
  where
  htmlDocumentToEventTarget = documentToEventTarget <<< htmlDocumentToDocument

foreign import waitLoaded :: forall e. Aff (dom :: DOM |e) Unit
foreign import onLoad :: forall e. Eff e Unit -> Eff e Unit
