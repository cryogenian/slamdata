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

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (random, RANDOM)
import Control.UI.Browser (hostAndProtocol, getHref)
import DOM (DOM)
import Data.List as L
import Data.URI (printURI, runParseURI)
import Data.URI.Types as URI
import OIDC.Crypt as Cryptography
import Quasar.Advanced.Types (ProviderR)
import Text.Parsing.StringParser (ParseError)

data Prompt = Login | None

type AuthenticationDetails =
  { uri ∷ String
  , unhashedNonce ∷ Cryptography.UnhashedNonce
  }

printPrompt ∷ Prompt → String
printPrompt =
  case _ of
    Login → "login"
    None → "none"

getRandomUnhashedNonce ∷ ∀ e. Eff (random ∷ RANDOM | e) Cryptography.UnhashedNonce
getRandomUnhashedNonce = Cryptography.UnhashedNonce ∘ show <$> random

getAuthenticationUri
  ∷ Prompt
  → Cryptography.UnhashedNonce
  → ProviderR
  → String
  → ∀ e. Eff (dom ∷ DOM, random ∷ RANDOM | e) (Either ParseError String)
getAuthenticationUri prompt unhashedNonce pr redirectURIStr = do
  key ← (Cryptography.KeyString ∘ show) <$> random
  hap ← hostAndProtocol
  hrefState ← map Cryptography.StateString getHref
  let authURIString = pr.openIDConfiguration.authorizationEndpoint
  for (runParseURI authURIString) \(URI.URI s h q f) →
    let
      nonce =
        Cryptography.hashNonce unhashedNonce
      query =
        pure
        $ URI.Query
        $ L.fromFoldable
        $ map (map Just)
          [ Tuple "response_type"  "id_token token"
          , Tuple "client_id" $ unwrap pr.clientId
          , Tuple "redirect_uri" redirectURIStr
          , Tuple "scope" "openid email"
          , Tuple "nonce" $ unwrap nonce
          , Tuple "prompt" $ printPrompt prompt
          , Tuple "display" "popup"
          ]
      uri = URI.URI s h query f
    in pure $ printURI uri
