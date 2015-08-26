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

module Utils where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Data.DOM.Simple.Document (body, setTitle)
import Data.DOM.Simple.Element (appendChild)
import Data.DOM.Simple.Events (addUIEventListener, UIEventType(..))
import Data.DOM.Simple.Types (HTMLElement(), DOMEvent(), DOMLocation())
import Data.Maybe (Maybe(..), isJust)
import Data.Array (elemIndex)
import DOM (DOM())
import Global (readFloat, isNaN, readInt)
import Data.Int (fromNumber)

import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Data.DOM.Simple.Window as W


onLoad :: forall e. Eff (dom :: DOM | e) Unit -> Eff (dom :: DOM |e) Unit 
onLoad action = do 
  addUIEventListener LoadEvent handler W.globalWindow 
  where
  handler :: DOMEvent -> _
  handler _ = action

bodyHTMLElement :: forall e. Eff (dom :: DOM | e) HTMLElement
bodyHTMLElement = W.document W.globalWindow >>= body 

mountUI :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit
mountUI node = bodyHTMLElement >>= flip appendChild node

locationString :: forall e. Eff (dom :: DOM | e) String 
locationString = W.location W.globalWindow >>= locationParent

setLocation :: forall e. String -> Eff (dom :: DOM | e) Unit
setLocation url = W.location W.globalWindow >>= W.setLocation url

setDocumentTitle :: forall e. String -> Eff (dom :: DOM | e) Unit
setDocumentTitle title = W.document W.globalWindow >>= setTitle title

foreign import newTab :: forall e. String -> Eff (dom :: DOM | e) Unit 
foreign import mailOpen :: forall e. String -> Eff (dom :: DOM | e) Unit
foreign import reload :: forall e. Eff (dom :: DOM | e) Unit
foreign import clearValue :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import select :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import replaceLocation :: forall e. String -> Eff (dom :: DOM | e) Unit
foreign import locationParent :: forall e. DOMLocation -> Eff (dom :: DOM | e) String

endsWith :: String -> String -> Boolean
endsWith needle haystack =
  isJust $ Str.indexOf' needle (Str.length haystack - Str.length needle) haystack

s2i :: String -> Maybe Int
s2i s =
  let n = readInt 10 s in
  if isNaN n 
  then Nothing
  else fromNumber n

s2n :: String -> Maybe Number
s2n s =
  let n = readFloat s in
  if isNaN n
  then Nothing
  else Just n

       
foreign import encodeURIComponent :: String -> String
foreign import decodeURIComponent :: String -> String 
