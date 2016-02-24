module OIDC.Aff where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (random, RANDOM())
import Control.UI.Browser (hostAndProtocol, getHref, setLocation)
import DOM (DOM())
import Data.Foldable as F
import Data.StrMap as Sm
import Data.Tuple (Tuple(..))
import Data.URI as URI
import Data.URI.Types as URI
import Global as Global
import OIDCCryptUtils as Cryptography
import Quasar.Auth as Auth
import Quasar.Auth.OpenIDConfiguration
import Quasar.Auth.Provider

requestAuthentication
  :: ProviderR
  -> forall e. Eff (dom :: DOM, random :: RANDOM | e) Unit
requestAuthentication pr = do
  csrf <- (Cryptography.KeyString <<< show) <$> random
  replay <- (Cryptography.UnhashedNonce <<< show) <$> random
  Auth.storeKeyString csrf
  Auth.storeNonce replay
  Auth.storeClientId pr.clientID
  hap <- hostAndProtocol
  hrefState <- map Cryptography.StateString getHref
  let authURIString =
        pr.openIDConfiguration
        # getOpenIDConfiguration
        # _.authorizationEndpoint
  -- The only way to get incorrect `authURIString` is incorrect config
  -- In this situation nothing happens.
  F.for_ (URI.runParseURI authURIString) \(URI.URI s h q f) ->
    let
      nonce =
        Cryptography.hashNonce replay
      redirectURIStr =
        hap <> SlamData.Config.redirectURIString
      state =
        Cryptography.bindState hrefState csrf
      query =
        pure
        $ URI.Query
        $ map pure
        $ map Global.encodeURIComponent
        $ Sm.fromFoldable
          [ Tuple "response_type"  "id_token token"
          , Tuple "client_id" $ Cryptography.runClientID pr.clientID
          , Tuple "redirect_uri" redirectURIStr
          , Tuple "scope" "openid email"
          , Tuple "state" $ Cryptography.runBoundStateJWS state
          , Tuple "nonce" $ Cryptography.runHashedNonce nonce
          ]
      uri = URI.URI s h query f
    in setLocation $ URI.printURI uri
