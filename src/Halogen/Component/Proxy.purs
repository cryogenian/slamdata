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
  ) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data ProxyQ i o a
  = Receive i a
  | Raise o a

type ProxyComponent i o m = H.Component HH.HTML (ProxyQ i o) i o m

proxy ∷ ∀ f i o m. H.Component HH.HTML f i o m → H.Component HH.HTML (ProxyQ i o) i o m
proxy component =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: HE.input Receive
    }
  where
  render ∷ i → H.ParentHTML (ProxyQ i o) f Unit m
  render i = HH.slot unit component i (HE.input Raise)

  eval ∷ ProxyQ i o ~> H.ParentDSL i (ProxyQ i o) f Unit o m
  eval = case _ of
    Receive i next → H.put i $> next
    Raise o next → H.raise o $> next

