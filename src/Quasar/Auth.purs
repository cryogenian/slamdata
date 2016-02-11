module Quasar.Auth
  ( IdToken()
  , authHeader
  , authed
  , retrieveIdToken
  ) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff())
import Data.Maybe as M
import DOM (DOM())
import Network.HTTP.RequestHeader

import Quasar.Auth.Route as R

newtype IdToken = IdToken String

authHeader
  :: IdToken
  -> RequestHeader
authHeader (IdToken tok) =
  RequestHeader
    "Authorization"
    ("Bearer " <> tok)


retrieveIdToken :: forall e. Eff (dom :: DOM | e) (M.Maybe IdToken)
retrieveIdToken =
  map (map IdToken) R.permissionsToken

authed
  :: forall a e
   . (M.Maybe IdToken -> Aff (dom :: DOM | e) a)
  -> Aff (dom :: DOM | e) a
authed f =
  liftEff retrieveIdToken
    >>= f
