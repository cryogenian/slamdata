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

module Utils.LocalStorage
  ( getLocalStorage
  , setLocalStorage
  , removeLocalStorage
  , onStorageEvent
  , StorageEvent
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonParser, encodeJson, printJson)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3, Fn2, runFn3, runFn2)
import Data.Maybe (Maybe(..), maybe)
import Unsafe.Coerce as U

import DOM (DOM)
import DOM.Event.Types (Event, EventType(..))
import DOM.Event.EventTarget as EventTarget
import DOM.HTML as DOMHTML
import DOM.HTML.Types as DOMHTMLTypes

foreign import setLocalStorageImpl
  ∷ forall eff. Fn2 String String (Eff (dom ∷ DOM | eff) Unit)
foreign import getLocalStorageImpl
  ∷ forall eff a . Fn3 (Maybe a) (a → Maybe a) String (Eff (dom ∷ DOM | eff) (Maybe String))
foreign import removeLocalStorageImpl
  ∷ forall eff . String → (Eff (dom ∷ DOM | eff)) Unit

type StorageEvent =
  { key ∷ String
  , oldValue ∷ String
  , newValue ∷ String
  , url ∷ String
  , storageArea ∷ Foreign
  }

eventToStorageEvent ∷ Event → StorageEvent
eventToStorageEvent = U.unsafeCoerce

setLocalStorage
  ∷ forall a eff g
  . (EncodeJson a, MonadEff (dom ∷ DOM | eff) g)
  ⇒ String → a → g Unit
setLocalStorage key val =
  liftEff
    $ runFn2 setLocalStorageImpl key
    $ printJson
    $ encodeJson val

getLocalStorage
  ∷ forall a eff g. (DecodeJson a, MonadEff (dom ∷ DOM | eff) g) ⇒ String → g (Either String a)
getLocalStorage key =
  liftEff
    $ maybe (Left $ "There is no value for key " <> key) (jsonParser >=> decodeJson)
    <$> runFn3 getLocalStorageImpl Nothing Just key

removeLocalStorage
  ∷ forall g eff. (MonadEff (dom ∷ DOM | eff) g) ⇒ String → g Unit
removeLocalStorage k =
  liftEff $ removeLocalStorageImpl k

onStorageEvent
  ∷ ∀ eff
  . (StorageEvent → Eff (dom ∷ DOM | eff) Unit)
  → Eff (dom ∷ DOM | eff) (Eff (dom ∷ DOM | eff) Unit)
onStorageEvent cb = do
  win ← DOMHTMLTypes.windowToEventTarget <$> DOMHTML.window
  let listener = EventTarget.eventListener (cb <<< eventToStorageEvent)
  EventTarget.addEventListener (EventType "storage") listener false win
  pure $ EventTarget.removeEventListener (EventType "storage") listener false win
