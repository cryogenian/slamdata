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

module OIDC.Aff where

import SlamData.Prelude

import Text.Parsing.StringParser (ParseError)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (random, RANDOM)
import Control.UI.Browser (hostAndProtocol, getHref)
import Data.StrMap as Sm
import Data.URI (printURI, runParseURI)
import Data.URI.Types as URI
import DOM (DOM)
import OIDCCryptUtils as Cryptography
import Quasar.Advanced.Types (ProviderR)
import SlamData.Quasar.Auth as Auth

requestAuthenticationURI
  ∷ ProviderR
  → String
  → ∀ e. Eff (dom ∷ DOM, random ∷ RANDOM | e) (Either ParseError String)
requestAuthenticationURI pr redirectURIStr = do
  csrf ← (Cryptography.KeyString ∘ show) <$> random
  replay ← (Cryptography.UnhashedNonce ∘ show) <$> random
  Auth.storeKeyString csrf
  Auth.storeNonce replay
  Auth.storeClientId pr.clientID
  hap ← hostAndProtocol
  hrefState ← map Cryptography.StateString getHref
  let authURIString = pr.openIDConfiguration.authorizationEndpoint
  for (runParseURI authURIString) \(URI.URI s h q f) →
    let
      nonce =
        Cryptography.hashNonce replay
      state =
        Cryptography.bindState hrefState csrf
      query =
        pure
        $ URI.Query
        $ map pure
        -- Here used to be encodeURIComponent. Removed it because it produced twice encoded uri.
        $ Sm.fromFoldable
          [ Tuple "response_type"  "id_token token"
          , Tuple "client_id" $ Cryptography.runClientID pr.clientID
          , Tuple "redirect_uri" redirectURIStr
          , Tuple "scope" "openid email"
          , Tuple "state" $ Cryptography.runBoundStateJWS state
          , Tuple "nonce" $ Cryptography.runHashedNonce nonce
          ]
      uri = URI.URI s h query f
    in pure $ printURI uri
