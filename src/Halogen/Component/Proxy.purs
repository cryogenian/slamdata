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

module Halogen.Component.Proxy
  ( ProxyQ
  , ProxyComponent
  , proxy
  , proxyQI
  , proxyQL
  , proxyQR
  , proxyTrans
  , proxy'
  ) where

import Prelude

import Data.Const (Const)
import Data.Coyoneda (Coyoneda, unCoyoneda)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM as HQ

data ProxyQ f i o a
  = Query (Coyoneda f a)
  | Receive i a
  | Raise o a

type ProxyComponent f i o m = H.Component HH.HTML (ProxyQ f i o) i o m

proxy
  ∷ ∀ f i o m
  . H.Component HH.HTML f i o m
  → H.Component HH.HTML (ProxyQ (Const Void) i o) i o m
proxy = proxy' (const (absurd <<< unwrap))

proxyQI
  ∷ ∀ f i o m
  . H.Component HH.HTML f i o m
  → H.Component HH.HTML (ProxyQ f i o) i o m
proxyQI = proxyTrans id

proxyQL
  ∷ ∀ f g i o m
  . H.Component HH.HTML (Coproduct f g) i o m
  → H.Component HH.HTML (ProxyQ f i o) i o m
proxyQL = proxyTrans left

proxyQR
  ∷ ∀ f g i o m
  . H.Component HH.HTML (Coproduct f g) i o m
  → H.Component HH.HTML (ProxyQ g i o) i o m
proxyQR = proxyTrans right

proxyTrans
  ∷ ∀ f g i o m
  . (g ~> f)
  → H.Component HH.HTML f i o m
  → H.Component HH.HTML (ProxyQ g i o) i o m
proxyTrans η = proxy' \k q ->
  H.query unit (η q) >>= case _ of
    Nothing -> HQ.halt "Proxy inner component query failed (this should be impossible)"
    Just a -> pure (k a)

proxy'
  ∷ ∀ f g i o m
  . (∀ a b. (b → a) → g b → H.ParentDSL i (ProxyQ g i o) f Unit o m a)
  → H.Component HH.HTML f i o m
  → H.Component HH.HTML (ProxyQ g i o) i o m
proxy' evalQuery component =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: HE.input Receive
    }
  where
  render ∷ i → H.ParentHTML (ProxyQ g i o) f Unit m
  render i = HH.slot unit component i (HE.input Raise)

  eval ∷ ProxyQ g i o ~> H.ParentDSL i (ProxyQ g i o) f Unit o m
  eval = case _ of
    Query iq → unCoyoneda evalQuery iq
    Receive i next → H.put i $> next
    Raise o next → H.raise o $> next
