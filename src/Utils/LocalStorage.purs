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
  ) where

import Prelude

import Control.Bind ((>=>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Coroutine as Coroutine
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Rec.Class as MonadRecClass

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonParser, encodeJson, printJson)
import Data.Either (Either(..))
import Data.Function (Fn3, Fn2, runFn3, runFn2)
import Data.Maybe (Maybe(..), maybe)
import Unsafe.Coerce as U

import DOM (DOM)
import DOM.Event.Types (Event, EventTarget, EventType(..))
import DOM.HTML as DOMHTML
import DOM.HTML.Types as DOMHTMLTypes

import Utils.DOM as DOMUtils

foreign import setLocalStorageImpl
  ∷ forall eff. Fn2 String String (Eff (dom ∷ DOM | eff) Unit)
foreign import getLocalStorageImpl
  ∷ forall eff a . Fn3 (Maybe a) (a → Maybe a) String (Eff (dom ∷ DOM | eff) (Maybe String))
foreign import removeLocalStorageImpl
  ∷ forall eff . String → (Eff (dom ∷ DOM | eff)) Unit
foreign import data Storage ∷ *

type StorageEvent =
  { target ∷ EventTarget
  , type ∷ String
  , bubbles ∷ Boolean
  , cancelable ∷ Boolean
  , key ∷ String
  , oldValue ∷ String
  , newValue ∷ String
  , url ∷ String
  , storageArea ∷ Storage
}

storageEventToEvent ∷ StorageEvent → Event
storageEventToEvent = U.unsafeCoerce

eventToStorageEvent ∷ Event → StorageEvent
eventToStorageEvent = U.unsafeCoerce

setLocalStorage
  ∷ forall a eff g
  . (EncodeJson a, MonadEff (dom ∷ DOM | eff) g)
  ⇒ String → a → g Unit
setLocalStorage  key val =
  liftEff $ runFn2 setLocalStorageImpl key $ printJson $ encodeJson val

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

storageEventProducer
  ∷ forall g eff
  . (MonadEff (dom ∷ DOM | eff) g)
  ⇒ Boolean → g (Coroutine.Producer StorageEvent (Aff (dom ∷ DOM, avar ∷ AVAR | eff)) Unit)
storageEventProducer capture =
  (_ Coroutine.$~ eventToStorageEventTransformer) <$> windowEventProducer "storage"
  where
  eventToStorageEventTransformer =
    MonadRecClass.forever $ Coroutine.transform eventToStorageEvent

  windowEventProducer s =
    DOMUtils.eventProducer (EventType s) capture
      <$> liftEff (DOMHTMLTypes.windowToEventTarget <$> DOMHTML.window)
