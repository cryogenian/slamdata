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

newtype IdToken = IdToken String

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
   . (M.Maybe IdToken -> Aff (dom :: DOM | e) a)
  -> Aff (dom :: DOM | e) a
authed f =
  liftEff retrieveIdToken
    >>= f
