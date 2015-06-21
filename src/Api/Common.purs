module Api.Common where

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Combinators ((~>), (:=))
import Data.Argonaut.Core (Json(), JAssoc(), jsonEmptyObject)
import Data.Int (fromNumber, toNumber)
import Data.Foldable (foldl)
import Network.HTTP.Affjax (Affjax(), AJAX())
import Network.HTTP.MimeType (MimeType(..), mimeTypeToString)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))

successStatus :: StatusCode
successStatus = StatusCode $ fromNumber 200

succeeded :: StatusCode -> Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = toNumber int

getResponse :: forall a e. String -> Affjax e a -> Aff (ajax :: AJAX | e) a
getResponse msg affjax = do
  res <- affjax
  if not $ succeeded res.status
    then throwError $ error msg
    else pure res.response

reqHeadersToJSON :: [RequestHeader] -> Json
reqHeadersToJSON = foldl go jsonEmptyObject
  where
  go obj (Accept mime) = "Accept" := mimeTypeToString mime ~> obj
  go obj (ContentType mime) = "Content-Type" := mimeTypeToString mime ~> obj
  go obj (RequestHeader k v) = k := v ~> obj
