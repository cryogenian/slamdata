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

module Quasar.Auth.OpenIDConfiguration
  ( OpenIDConfiguration(..)
  , OpenIDConfigurationR()
  , getOpenIDConfiguration
  ) where

import Prelude
import Data.Argonaut ((.?))
import Data.Argonaut as JS

import OIDCCryptUtils.JSONWebKey as JWK
import OIDCCryptUtils.Types as OIDC

type OpenIDConfigurationR =
  { issuer :: OIDC.Issuer
  , authorizationEndpoint :: String
  , tokenEndpoint :: String
  , userinfoEndpoint :: String
  , jwks :: Array JWK.JSONWebKey
  }

newtype OpenIDConfiguration = OpenIDConfiguration OpenIDConfigurationR

getOpenIDConfiguration
  :: OpenIDConfiguration
  -> OpenIDConfigurationR
getOpenIDConfiguration (OpenIDConfiguration conf) =
  conf

instance decodeJsonOpenIDConfiguration :: JS.DecodeJson OpenIDConfiguration where
  decodeJson json = do
    obj <- JS.decodeJson json
    issuer <- OIDC.Issuer <$> obj .? "issuer"
    authorizationEndpoint <- obj .? "authorization_endpoint"
    tokenEndpoint <- obj .? "token_endpoint"
    userinfoEndpoint <- obj .? "userinfo_endpoint"
    jwks <- obj .? "jwks"
    pure $ OpenIDConfiguration
      { issuer
      , authorizationEndpoint
      , tokenEndpoint
      , userinfoEndpoint
      , jwks
      }
