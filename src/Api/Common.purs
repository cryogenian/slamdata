module Api.Common where

import Prelude
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Combinators ((~>), (:=))
import Data.Argonaut.Core (Json(), JAssoc(), jsonEmptyObject)
import Data.Int (fromNumber, toNumber)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax.Request (Requestable)
import Network.HTTP.Affjax.Response (Respondable)
import Network.HTTP.Affjax (Affjax(), AJAX(), URL(), AffjaxRequest(), defaultRequest, affjax, retry)
import Network.HTTP.Method (Method(..))

import Network.HTTP.MimeType (MimeType(..), mimeTypeToString)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))

successStatus :: StatusCode
successStatus = StatusCode 200

succeeded :: StatusCode -> Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = int

-- | A version of `affjax` with our retry policy.
slamjax :: forall e a b. (Requestable a, Respondable b) => AffjaxRequest a -> Affjax (avar :: AVAR | e) b
slamjax = retry Nothing affjax

retryGet :: forall e a. (Respondable a) => URL -> Affjax (avar :: AVAR | e) a
retryGet u = slamjax $ defaultRequest { url = u }

retryDelete :: forall e a. (Respondable a) => URL -> Affjax (avar :: AVAR | e) a
retryDelete u = slamjax $ defaultRequest { url = u, method = DELETE }

retryPost :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax (avar :: AVAR | e) b
retryPost u c = slamjax $ defaultRequest { method = POST, url = u, content = Just c }

retryPut :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax (avar :: AVAR | e) b
retryPut u c = slamjax $ defaultRequest { method = PUT, url = u, content = Just c }

getResponse :: forall a e. String -> Affjax e a -> Aff (ajax :: AJAX | e) a
getResponse msg affjax = do
  res <- affjax
  if not $ succeeded res.status
    then throwError $ error msg
    else pure res.response

reqHeadersToJSON :: Array RequestHeader -> Json
reqHeadersToJSON = foldl go jsonEmptyObject
  where
  go obj (Accept mime) = "Accept" := mimeTypeToString mime ~> obj
  go obj (ContentType mime) = "Content-Type" := mimeTypeToString mime ~> obj
  go obj (RequestHeader k v) = k := v ~> obj
