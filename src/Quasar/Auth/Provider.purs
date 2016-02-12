module Quasar.Auth.Provider
  ( Provider(..)
  , ProviderR()
  , getProvider
  ) where

import Prelude
import Data.Argonaut ((.?))
import Data.Argonaut as JS
import Quasar.Auth.OpenIDConfiguration

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
