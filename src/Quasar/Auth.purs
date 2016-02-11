module Quasar.Auth
  ( authHeader
  , authed
  , retrieveIdToken
  , module OIDCCryptUtils.Types
  ) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff())
import Data.Maybe as M
import DOM (DOM())
import Network.HTTP.RequestHeader
import Quasar.Auth.Permission as P
import OIDCCryptUtils.Types (IdToken(..))

authHeader
  :: IdToken
  -> RequestHeader
authHeader (IdToken tok) =
  RequestHeader
    "Authorization"
    ("Bearer " <> tok)


-- | TODO!!
retrieveIdToken :: forall e. Eff (dom :: DOM | e) (M.Maybe IdToken)
retrieveIdToken =
  pure M.Nothing

authed
  :: forall a e
   . (M.Maybe IdToken -> Array P.Permission -> Aff (dom :: DOM | e) a)
  -> Aff (dom :: DOM | e) a
authed f = do
  idToken <- liftEff retrieveIdToken
  perms <- liftEff P.retrievePermissions
  f idToken perms
