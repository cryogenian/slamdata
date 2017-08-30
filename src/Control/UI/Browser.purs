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
  , setLocation
  , locationString
  , select
  , newTab
  , reload
  , setTitle
  , hostAndProtocol
  , getHref
  , setHref
  , detectEmbedding
  ) where

import Prelude

import Control.Monad.Eff (Eff)

import DOM (DOM)
import DOM.HTML.Types (HTMLElement, Location)
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window

locationObject ∷ ∀ e. Eff (dom ∷ DOM | e) Location
locationObject =
  window
    >>= Window.location

replaceLocation ∷ ∀ e. String → Eff (dom ∷ DOM | e) Unit
replaceLocation str =
  locationObject
    >>= Location.replace str

setLocation ∷ ∀ e. String → Eff (dom ∷ DOM | e) Unit
setLocation str =
  locationObject
    >>= Location.assign str

reload ∷ ∀ e. Eff (dom ∷ DOM | e) Unit
reload =
  locationObject
    >>= Location.reload

hostAndProtocol ∷ ∀ e. Eff (dom ∷ DOM |e) String
hostAndProtocol = do
  loc <- locationObject
  protocol <- Location.protocol loc
  host <- Location.host loc
  pure $ protocol <> "//" <> host

getHref ∷ ∀ e. Eff (dom ∷ DOM|e) String
getHref = locationObject >>= Location.href

setHref ∷ ∀ e. String → Eff (dom ∷ DOM|e) Unit
setHref str =
  locationObject
    >>= Location.setHref str

foreign import locationString ∷ ∀ e. Eff (dom ∷ DOM | e) String
foreign import select ∷ ∀ e. HTMLElement → Eff (dom ∷ DOM | e) Unit
foreign import newTab ∷ ∀ e. String → Eff (dom ∷ DOM | e) Unit
foreign import setTitle ∷ ∀ e. String → Eff (dom ∷ DOM | e) Unit

-- | Checks whether the current page is embedded in another page via iframe.
foreign import detectEmbedding ∷ ∀ e. Eff (dom ∷ DOM | e) Boolean
