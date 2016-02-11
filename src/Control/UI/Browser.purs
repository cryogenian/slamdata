{-
Copyright 2016 SlamData, Inc.

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

module Control.UI.Browser
  ( locationObject
  , replaceLocation
  , decodeURIComponent
  , encodeURIComponent
  , setLocation
  , getLocation
  , getHash
  , locationString
  , modifyLocation
  , modifyLocationM
  , alterLocation
  , alterLocationM
  , select
  , newTab
  , clearValue
  , reload
  , setTitle
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.String as Str

import DOM (DOM())
import DOM.HTML.Types (HTMLElement(), Location())
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window


locationObject :: forall e. Eff (dom :: DOM | e) Location
locationObject =
  window
    >>= Window.location

replaceLocation :: forall e. String -> Eff (dom :: DOM | e) Unit
replaceLocation str = do
  locationObject
    >>= Location.replace str

setLocation :: forall e. String -> Eff (dom :: DOM | e) Unit
setLocation str =
  locationObject
    >>= Location.assign str

getLocation :: forall e. Eff (dom :: DOM |e) String
getLocation =
  locationObject
    >>= Location.href

getHash :: forall e. Eff (dom :: DOM|e) String
getHash =
  locationObject
    >>= Location.hash

-- | Uses `setLocation`
modifyLocation :: forall e. (String -> String) -> Eff (dom :: DOM |e) Unit
modifyLocation fn = do
  old <- getLocation
  setLocation $ fn old

-- | Uses `replaceLocation`
alterLocation :: forall e. (String -> String) -> Eff (dom :: DOM|e) Unit
alterLocation fn = do
  old <- getLocation
  replaceLocation $ fn old

-- | Same as `modifyLocation` but mutating function is effectful
modifyLocationM
  :: forall e. (String -> Eff (dom :: DOM|e) String) -> Eff (dom :: DOM|e) Unit
modifyLocationM fnM = do
  getLocation
    >>= fnM
    >>= setLocation

-- | Same as `alterLocation` but mutating function is effectful
alterLocationM
  :: forall e. (String -> Eff (dom :: DOM|e) String) -> Eff (dom :: DOM|e) Unit
alterLocationM fnM =
  getLocation
    >>= fnM
    >>= replaceLocation

reload :: forall e. Eff (dom :: DOM | e) Unit
reload =
  locationObject
    >>= Location.reload

foreign import locationString :: forall e. Eff (dom :: DOM | e) String
foreign import select :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import newTab :: forall e. String -> Eff (dom :: DOM | e) Unit
foreign import clearValue :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import setTitle :: forall e. String -> Eff (dom :: DOM | e) Unit

foreign import decodeURIComponent :: String -> String
foreign import encodeURIComponent :: String -> String
