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

module Control.UI.Browser.Event (raiseEvent) where

import Control.Monad.Eff (Eff())

import Data.Function (Fn2(), runFn2)

import DOM (DOM())
import DOM.HTML.Types (HTMLElement())


foreign import raiseEventImpl
  :: forall eff
   . Fn2 String HTMLElement (Eff (dom :: DOM | eff) HTMLElement)


raiseEvent :: forall e. String -> HTMLElement -> Eff (dom :: DOM |e) HTMLElement
raiseEvent name el = runFn2 raiseEventImpl name el
