{-
Copyright 2015 SlamData, Inc.

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

module Api.Common where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF())
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Combinators ((~>), (:=))
import Data.Argonaut.Core (Json(), JAssoc(), jsonEmptyObject)
import Data.Date (nowEpochMilliseconds)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy
import Data.Time (Milliseconds(..))
import Model.Path (AnyPath())
import Network.HTTP.Affjax (Affjax(), AJAX(), URL(), AffjaxRequest(), RetryPolicy(), defaultRequest, affjax, retry, defaultRetryPolicy)
import Network.HTTP.Affjax.Request (Requestable)
import Network.HTTP.Affjax.Response (Respondable)
import Network.HTTP.Method (Method(..))
import Network.HTTP.MimeType (MimeType(..), mimeTypeToString)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import qualified Data.String as S

successStatus :: StatusCode
successStatus = StatusCode 200

notFoundStatus :: StatusCode
notFoundStatus = StatusCode 404

succeeded :: StatusCode -> Boolean
succeeded (StatusCode int) =
  200 <= code && code < 300
  where code = int

type RetryEffects e = (avar :: AVAR, ref :: REF | e)

-- | A version of `affjax` with our retry policy.
slamjax :: forall e a b. (Requestable a, Respondable b) => AffjaxRequest a -> Affjax (RetryEffects e) b
slamjax = retry defaultRetryPolicy affjax

retryGet :: forall e a fd. (Respondable a) => Path Abs fd Sandboxed -> Affjax (RetryEffects e) a
retryGet =
  getWithPolicy { shouldRetryWithStatusCode: not <<< succeeded
                , delayCurve: const 1000
                , timeout: Just 30000
                }

getOnce :: forall e a fd. (Respondable a) => Path Abs fd Sandboxed -> Affjax (RetryEffects e) a
getOnce = getWithPolicy defaultRetryPolicy


getWithPolicy :: forall e a fd. (Respondable a) => RetryPolicy -> Path Abs fd Sandboxed -> Affjax (RetryEffects e) a
getWithPolicy policy u = do
  nocache <- liftEff $ nowEpochMilliseconds
  let url = printPath u
      symbol = if S.contains "?" url then "&" else "?"
  retry policy affjax defaultRequest { url = url ++ symbol ++ "nocache=" ++ pretty nocache }
  where
  pretty (Milliseconds ms) =
    let s = show ms
    in fromMaybe s (S.stripSuffix ".0" s)

retryDelete :: forall e a fd. (Respondable a) => Path Abs fd Sandboxed -> Affjax (RetryEffects e) a
retryDelete u = slamjax $ defaultRequest { url = printPath u, method = DELETE }

retryPut :: forall e a b fd. (Requestable a, Respondable b) => Path Abs fd Sandboxed -> a -> MimeType -> Affjax (RetryEffects e) b
retryPut u c mime = slamjax $ defaultRequest { method = PUT, url = printPath u, content = Just c, headers = [ContentType mime] }

getResponse :: forall a e. String -> Affjax e a -> Aff (ajax :: AJAX | e) a
getResponse msg affjax = do
  res <- attempt affjax
  case res of
    Left e -> throwError $ error msg
    Right r -> do
      if not $ succeeded r.status
        then throwError $ error msg
        else pure r.response

reqHeadersToJSON :: Array RequestHeader -> Json
reqHeadersToJSON = foldl go jsonEmptyObject
  where
  go obj (Accept mime) = "Accept" := mimeTypeToString mime ~> obj
  go obj (ContentType mime) = "Content-Type" := mimeTypeToString mime ~> obj
  go obj (RequestHeader k v) = k := v ~> obj

ldJSON :: MimeType
ldJSON = MimeType "application/ldjson"
