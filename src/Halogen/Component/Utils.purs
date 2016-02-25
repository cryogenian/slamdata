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

module Halogen.Component.Utils where

import Prelude (Applicative, pure, Unit(), unit, bind, ($), zero, const)

import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling as SCR

import Control.Monad.Aff (Aff(), Canceler(), forkAff, later', runAff)
import Control.Monad.Aff.AVar (AVAR(), makeVar, putVar, takeVar)

import Data.Functor.Aff (liftAff)
import Data.Functor.Eff (liftEff)
import Data.Either as E
import Data.Time (Milliseconds(..))

import Halogen as H --(ComponentDSL(), ParentDSL(), ChildF(..), liftH, subscribe)
import Halogen.Query.EventSource as He

applyCF :: forall a b c i. (a -> b i -> c) -> H.ChildF a b i -> c
applyCF fn (H.ChildF a b) = fn a b

forceRerender
  :: forall s f g
   . (Applicative g)
  => H.ComponentDSL s f g Unit
forceRerender = H.liftH (pure unit)

forceRerender'
  :: forall s s' f f' g p
   . (Applicative g)
  => H.ParentDSL s s' f f' g p Unit
forceRerender' = H.liftH (H.liftH (pure unit))


liftWithCanceler
  :: forall s f e a
   . (Canceler (avar :: AVAR|e)-> Unit -> f Unit)
  -> Aff (avar :: AVAR|e) a
  -> H.ComponentDSL s f (Aff (avar :: AVAR|e)) a
liftWithCanceler f aff = do
  v <- liftAff makeVar
  canceler <- liftAff $ forkAff do
    res <- aff
    putVar v res
  sendAfter zero $ f canceler unit
  liftAff $ takeVar v

liftWithCanceler'
  :: forall s s' f f' p a e
   . (Canceler (avar :: AVAR|e) -> Unit -> f Unit)
  -> Aff (avar :: AVAR|e) a
  -> H.ParentDSL s s' f f' (Aff (avar :: AVAR|e)) p a
liftWithCanceler' f aff = do
  v <- liftAff makeVar
  canceler <- liftAff $ forkAff do
    res <- aff
    putVar v res
  sendAfter' zero $ f canceler unit
  liftAff $ takeVar v

sendAfter
  :: forall s f e
   . Milliseconds
  -> f Unit
  -> H.ComponentDSL s f (Aff (avar :: AVAR|e)) Unit
sendAfter ms action =
  H.subscribe $ oneTimeEventSource ms action

sendAfter'
  :: forall s s' f f' p a e
   . Milliseconds
  -> f Unit
  -> H.ParentDSL s s' f f' (Aff (avar :: AVAR|e)) p Unit
sendAfter' ms action =
  H.subscribe' $ oneTimeEventSource ms action

oneTimeEventSource
  :: forall f e
   . Milliseconds
  -> f Unit
  -> He.EventSource f (Aff (avar :: AVAR|e))
oneTimeEventSource (Milliseconds n) action =
  He.EventSource
  $ SCR.producerToStallingProducer
  $ produce \emit ->
      runAff (const $ pure unit) (const $ pure unit)
      $ later' (Data.Int.floor $ Math.max n zero)
      $ liftEff do
        emit $ E.Left action
        emit $ E.Right unit
