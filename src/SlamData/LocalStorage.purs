{-
Copyright 2017 SlamData, Inc.

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

module SlamData.LocalStorage (Key(..), LocalStorageF(..), run) where

import SlamData.Prelude

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.AVar (AVar, AVAR)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Throw (note)
import DOM (DOM)
import DOM.Event.EventTarget as EventTarget
import DOM.Event.Types (Event, EventType(..))
import DOM.HTML as DOMHTML
import DOM.HTML as HTML
import DOM.HTML.Types as DOMHTMLTypes
import DOM.HTML.Window as Window
import DOM.WebStorage.Storage as Storage
import DOM.WebStorage.Event.StorageEvent (StorageEvent)
import Data.Argonaut (Json)
import Data.Argonaut as Argonaut
import Data.Argonaut.Core as ArgonautCore
import Data.Foreign (MultipleErrors)
import Data.Nullable as Nullable
import Utils.StorageEvent as StorageEventUtils

newtype Key a = Key String

derive instance keyNewtype ∷ Newtype (Key a) _

data LocalStorageF a b
  = Persist (b -> Json) (Key b) b a
  | Retrieve (Json -> Either String b) (Key b) (Either String b -> a)
  | AwaitChange (Json -> Either String b) (Key b) (b → a)
  | Remove (Key b) a

run :: forall a b eff. LocalStorageF a b -> Aff (dom :: DOM, avar :: AVAR | eff) a
run = case _ of
  Retrieve decode key k →
    k <<< (decode <=< Argonaut.jsonParser <=< note ("No key " <> unwrap key <> " in LocalStorage"))
      <<< Nullable.toMaybe
      <$> (liftEff $ Storage.getItem (unwrap key) =<< Window.localStorage =<< HTML.window)
  Persist encode key json a → do
    liftEff
      $ HTML.window
      >>= Window.localStorage
      >>= Storage.setItem (unwrap key) (ArgonautCore.stringify $ encode json)
    pure a
  Remove key a → do
    liftEff
      $ HTML.window
      >>= Window.localStorage
      >>= Storage.removeItem (unwrap key)
    pure a
  AwaitChange decode key k → do
    newValueAVar ← AVar.makeVar
    win ← liftEff $ DOMHTMLTypes.windowToEventTarget <$> DOMHTML.window
    let listener =
          EventTarget.eventListener \event →
            for_ (fromEvent event) \event' →
              when (StorageEventUtils.keyEq (unwrap key) event')
                $ for_ (StorageEventUtils.decodeNewValue' decode event') \newValue →
                  putVar newValueAVar newValue
                    *> EventTarget.removeEventListener (EventType "storage") listener false win
    liftEff $ EventTarget.addEventListener (EventType "storage") listener false win
    k <$> AVar.takeVar newValueAVar

-- Ok to runAff here without an error handler as putVar is non blocking and we
-- never cancel this AVar. Void is applied as the canceller is unusable.
putVar ∷ forall a eff. AVar a → a → Eff (avar ∷ AVAR | eff) Unit
putVar avar =
  void ∘ runAff (const $ pure unit) (const $ pure unit) ∘ AVar.putVar avar

-- Needs a foldable instance so can't be forall m. MonadError MultipleErrors m => m StorageEvent
fromEvent ∷ Event → Either MultipleErrors StorageEvent
fromEvent = StorageEventUtils.fromEvent

