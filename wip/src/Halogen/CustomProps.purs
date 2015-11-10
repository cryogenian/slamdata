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

module Halogen.CustomProps where

import Prelude

import Data.ExistsR
import Data.Functor (($>))
import Data.Maybe (Maybe(..))
import Halogen.HTML.Core
import Halogen.HTML.Events (onSubmit, EventProp())
import Halogen.HTML.Events.Handler (EventHandler(), preventDefault)
import Halogen.HTML.Events.Types (Event(), KeyboardEvent(), MouseEvent())

type MbEventProp e i = (Event e -> EventHandler (Maybe i)) -> Prop i


attr :: forall i. Maybe Namespace -> AttrName -> String -> Prop i
attr = Attr

readonly :: forall i. Prop i
readonly = Attr Nothing (attrName "readonly") "true"

dataZClipboard :: forall i. String -> Prop i
dataZClipboard = Attr Nothing (attrName "data-zclipboard")

mbHandler :: forall fields i. EventName fields -> MbEventProp fields i
mbHandler name k = Handler (mkExistsR (HandlerF name k))

nonSubmit :: forall i. Prop i
nonSubmit = mbHandler (eventName "submit") (\_ -> preventDefault $> Nothing)

mbKeyPress :: forall i. MbEventProp KeyboardEvent i
mbKeyPress = mbHandler (eventName "keypress")

mbKeyDown :: forall i. MbEventProp KeyboardEvent i
mbKeyDown = mbHandler (eventName "keydown")

mbClick :: forall i. MbEventProp MouseEvent i
mbClick = mbHandler (eventName "click")

onPaste :: forall e i. EventProp e i
onPaste = handler (eventName "paste")

ariaLabel :: forall i. String -> Prop i
ariaLabel label = Attr Nothing (attrName "aria-label") label

frameBorder :: forall i. Int -> Prop i
frameBorder = Attr Nothing (attrName "frameBorder") <<< show
