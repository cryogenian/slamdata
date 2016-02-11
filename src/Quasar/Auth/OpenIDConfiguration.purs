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
