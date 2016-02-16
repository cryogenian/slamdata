module SlamData.AuthRedirect
  ( main
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff as Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Maybe.Trans as MBT
import Control.Monad.Trans as MT
import Control.MonadPlus (guard)
import Control.Plus (empty)

import Data.Either as E
import Data.Foldable as F
import Data.Maybe as M

import DOM as DOM
import DOM.HTML as DOM
import DOM.HTML.Window as Win
import DOM.HTML.Location as Loc

import Network.HTTP.Affjax as AX

import OIDCCryptUtils as OIDC

import SlamData.AuthRedirect.RedirectHashPayload as Payload
import Quasar.Aff as Quasar
import Quasar.Auth as Auth
import Quasar.Auth.OpenIDConfiguration as Auth
import Quasar.Auth.Provider as Auth

type RedirectEffects =
  Quasar.RetryEffects
    ( console :: Console.CONSOLE
    , err :: Exn.EXCEPTION
    , dom :: DOM.DOM
    , rsaSignTime :: OIDC.RSASIGNTIME
    , ajax :: AX.AJAX
    )

type RedirectState =
  { payload :: Payload.RedirectHashPayload
  , keyString :: OIDC.KeyString
  , unhashedNonce :: OIDC.UnhashedNonce
  , clientID :: OIDC.ClientID
  }

retrieveRedirectState :: Eff RedirectEffects RedirectState
retrieveRedirectState = do
  hash <- DOM.window >>= Win.location >>= Loc.hash

  payload <-
    Payload.parseUriHash hash #
      E.either (Exn.throw <<< show) pure

  keyString <-
    Auth.retrieveKeyString >>=
      M.maybe (Exn.throw "Failed to retrieve KeyString from local storage") pure

  unhashedNonce <-
    Auth.retrieveNonce >>=
      M.maybe (Exn.throw "Failed to retrieve UnhashedNonce from local storage") pure

  clientID <-
    Auth.retrieveClientID >>=
      M.maybe (Exn.throw "Failed to retrieve ClientID from local storage") pure

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
    # MT.lift
    >>= guard
  -- If the IdToken has been verified,
  -- then we may proceed to extract the redirect URL.

  OIDC.unbindState st.payload.state st.keyString
    <#> OIDC.runStateString
    >>> RedirectURL
      # pure
      # MBT.MaybeT



main :: Eff RedirectEffects Unit
main = do
  -- We're getting token too fast. It isn't valid until next second (I think)
  Aff.runAff Exn.throwException (\_ -> pure unit)  do
    state <- liftEff retrieveRedirectState
    -- First, retrieve the provider that matches our stored ClientID.
    Auth.Provider provider <- do
      providers <-
        Quasar.retrieveAuthProviders
          >>= M.maybe
                (liftEff
                 $ Exn.throw "Failed to retrieve auth providers from Quasar")
                pure

      F.find (\(Auth.Provider pr) -> pr.clientID == state.clientID) providers
        # M.maybe
            (liftEff
             $ Exn.throw
             $ "Could not find provider matching client ID '"
             <> OIDC.runClientID state.clientID <> "'")
            pure

    let openIDConfiguration =
          Auth.getOpenIDConfiguration provider.openIDConfiguration

    liftEff do
      -- Try to verify the IdToken against each of the provider's jwks,
      -- stopping at the first success.
      RedirectURL redirectURL <-
        openIDConfiguration.jwks
          <#> verifyRedirect state openIDConfiguration.issuer
            # F.foldl ((<|>)) empty
            # MBT.runMaybeT
          >>= M.maybe (Exn.throw "Failed to verify redirect") pure


      Auth.storeIdToken state.payload.idToken
      DOM.window
        >>= Win.location
        >>= Loc.setHref redirectURL
