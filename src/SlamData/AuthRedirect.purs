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

module SlamData.AuthRedirect
  ( main
  ) where

import SlamData.Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now as Now
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Maybe.Trans as MBT
import Control.Monad.Eff.Random (RANDOM)

import DOM as DOM
import DOM.HTML (window)
import DOM.HTML.Location as Loc
import DOM.HTML.Window as Win

import Network.HTTP.Affjax as AX

import OIDC.Crypt as OIDC

import SlamData.AuthRedirect.RedirectHashPayload as Payload
import SlamData.Quasar.Auth.Store as AuthStore
import SlamData.Quasar.Auth.Retrieve as AuthRetrieve

import Utils.DOM as DOMUtils

type RedirectEffects =
  ( ajax :: AX.AJAX
  , avar :: AVar.AVAR
  , dom :: DOM.DOM
  , random :: RANDOM
  , err :: Exn.EXCEPTION
  , now :: Now.NOW
  , ref ∷ Ref.REF
  , rsaSignTime :: OIDC.RSASIGNTIME
  )

type RedirectState =
  { payload :: Payload.RedirectHashPayload
  , keyString :: OIDC.KeyString
  , unhashedNonce :: OIDC.UnhashedNonce
  , clientID :: OIDC.ClientID
  }

retrieveRedirectState :: Eff RedirectEffects RedirectState
retrieveRedirectState = do
  hash ← window >>= Win.location >>= Loc.hash

  payload ←
    Payload.parseUriHash hash #
      either (fail ∘ show) pure

  keyString ←
    AuthRetrieve.retrieveKeyString >>=
      maybe (fail "Failed to retrieve KeyString from local storage") pure

  unhashedNonce ←
    AuthRetrieve.retrieveNonce >>=
      maybe (fail "Failed to retrieve UnhashedNonce from local storage") pure

  clientID ←
    AuthRetrieve.retrieveClientID >>=
      maybe (fail "Failed to retrieve ClientID from local storage") pure

  pure
    { payload
    , keyString
    , unhashedNonce
    , clientID
    }

newtype RedirectURL = RedirectURL String

verifyRedirect
  :: RedirectState
  -> OIDC.Issuer
  -> OIDC.JSONWebKey
  -> MBT.MaybeT (Eff RedirectEffects) RedirectURL
verifyRedirect st issuer jwk = do
  -- Fail immediately if the IdToken fails to verify.
  OIDC.verifyIdToken st.payload.idToken issuer st.clientID st.unhashedNonce jwk
    # lift
    >>= guard
  -- If the IdToken has been verified,
  -- then we may proceed to extract the redirect URL.

  OIDC.unbindState st.payload.state st.keyString
    <#> OIDC.runStateString
    >>> RedirectURL
      # pure
      # MBT.MaybeT

fail :: ∀ a. String → Eff RedirectEffects a
fail s = AuthStore.storeIdToken (Left s) *> (window >>= DOMUtils.close) *> Exn.throw s

main :: Eff RedirectEffects Unit
main = do
  -- We're getting token too fast. It isn't valid until next second (I think)
  void $ Aff.runAff Exn.throwException (const (pure unit)) do
    state ← liftEff retrieveRedirectState
    -- First, retrieve the provider that matches our stored ClientID.
    maybeOpenIDConfiguration ← liftEff $ map _.openIDConfiguration <$> AuthRetrieve.retrieveProviderR

    case maybeOpenIDConfiguration of
      Just openIDConfiguration →
        liftEff do
          -- Try to verify the IdToken against each of the provider's jwks,
          -- stopping at the first success.
          RedirectURL redirectURL ←
            openIDConfiguration.jwks
              <#> verifyRedirect state openIDConfiguration.issuer
                # foldl ((<|>)) empty
                # MBT.runMaybeT
              >>= maybe (fail "Failed to verify redirect") pure

          AuthStore.storeIdToken $ Right state.payload.idToken
      Nothing →
        liftEff $ fail "Failed to retrieve provider from local storage"
    liftEff $ window >>= DOMUtils.close
