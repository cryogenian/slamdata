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

module Utils.LocalStorage (getLocalStorage, setLocalStorage)  where

import Prelude

import Control.Bind ((>=>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff, MonadEff)
import Data.Argonaut
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Function
import DOM (DOM())

foreign import setLocalStorageImpl
  :: forall e. Fn2 String String (Eff (dom :: DOM|e) Unit)
foreign import getLocalStorageImpl
  :: forall e a
   . Fn3 (Maybe a) (a -> Maybe a) String (Eff (dom :: DOM|e) (Maybe String))

setLocalStorage
  :: forall a e g
   . (EncodeJson a, MonadEff (dom :: DOM|e) g) => String -> a -> g Unit
setLocalStorage  key val =
  liftEff $ runFn2 setLocalStorageImpl key $ printJson $ encodeJson val

getLocalStorage
  :: forall a e g
   . (DecodeJson a, MonadEff (dom :: DOM|e) g) => String -> g (Either String a)
getLocalStorage key =
  liftEff
  $ maybe (Left $ "There is no value in key " <> key) (jsonParser >=> decodeJson)
  <$> runFn3 getLocalStorageImpl Nothing Just key
