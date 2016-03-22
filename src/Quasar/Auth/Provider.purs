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

module Quasar.Auth.Provider
  ( Provider(..)
  , ProviderR
  , getProvider
  ) where

import Prelude
import Data.Argonaut ((.?))
import Data.Argonaut as JS
import Quasar.Auth.OpenIDConfiguration (OpenIDConfiguration)

import OIDCCryptUtils.Types as OIDC

type ProviderR =
  { displayName :: String
  , clientID :: OIDC.ClientID
  , openIDConfiguration :: OpenIDConfiguration
  }

newtype Provider = Provider ProviderR

getProvider
  :: Provider
  -> ProviderR
getProvider (Provider prv) =
  prv

instance decodeJsonProvider :: JS.DecodeJson Provider where
  decodeJson json = do
    obj <- JS.decodeJson json
    displayName <- obj .? "display_name"
    clientID <- OIDC.ClientID <$> obj .? "client_id"
    openIDConfiguration <- obj .? "openid_configuration"
    pure $ Provider
      { displayName
      , clientID
      , openIDConfiguration
      }
