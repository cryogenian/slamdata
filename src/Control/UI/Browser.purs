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

module Control.UI.Browser where

import Prelude
import Control.Monad.Eff (Eff())
import DOM (DOM())
import DOM.HTML.Types (HTMLElement())

foreign import replaceLocation :: forall e. String -> Eff (dom :: DOM |e) Unit
foreign import decodeURIComponent :: String -> String
foreign import encodeURIComponent :: String -> String
foreign import setLocation :: forall e. String -> Eff (dom :: DOM |e) Unit
foreign import locationString :: forall e. Eff (dom :: DOM |e) String
foreign import select :: forall e. HTMLElement -> Eff (dom :: DOM |e) Unit
foreign import newTab :: forall e. String -> Eff (dom :: DOM |e) Unit
foreign import clearValue :: forall e. HTMLElement -> Eff (dom :: DOM|e) Unit
foreign import reload :: forall e. Eff (dom :: DOM|e) Unit
foreign import setTitle :: forall e. String -> Eff (dom :: DOM|e) Unit
