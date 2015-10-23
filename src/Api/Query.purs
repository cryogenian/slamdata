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

module Api.Query (query, query', port, sample, SQL(), fields, count, all, templated) where

import Prelude
import Api.Common (RetryEffects(), getResponse, succeeded, retryGet, slamjax, ldJSON)
import Config.Paths (queryUrl, dataUrl)
import Control.Apply (lift2)
import Control.Bind ((<=<), (>=>))
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.MonadPlus (guard)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (JArray(), Json(), JObject(), jsonEmptyObject, isArray, isObject, foldJson, toArray, toObject, toNumber, fromObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concat, nub, filter, head)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Foldable (foldl)
import Data.List (uncons, fromList)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Path.Pathy (rootDir, (</>), dir, file, printPath)
import Data.String (split, replace)
import Data.StrMap (StrMap(), keys, toList, empty, lookup)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (lmap)
import Model.Notebook.Port (VarMapValue())
import Model.Path (FilePath(), rootify)
import Model.Resource (Resource(..), resourcePath, isFile, _name, resourceDir, resourceName)
import Network.HTTP.Affjax (Affjax(), AJAX(), affjax, defaultRequest)
import Network.HTTP.Method (Method(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Optic.Core
import Utils (encodeURIComponent)
import qualified Data.Int as I

-- | This is template string where actual path is encoded like {{path}}
type SQL = String

query :: forall e. Resource -> SQL -> Aff (RetryEffects (ajax :: AJAX | e)) JArray
query res sql =
  if not $ isFile res
  then pure []
  else extractJArray <$> (getResponse msg $ retryGet uriPath)
  where
  msg = "error in query"
  uriPath = mkURI res sql

query' :: forall e. Resource -> SQL -> Aff (RetryEffects (ajax :: AJAX | e)) (Either String JArray)
query' res@(File _) sql = do
  result <- retryGet (mkURI' res sql)
  pure if succeeded result.status
       then Right (extractJArray result.response)
       else readError "error in query" result.response

query' _ _ = pure $ Left "Query resource is not a file"

count :: forall e. Resource -> Aff (RetryEffects (ajax :: AJAX | e)) Int
count res = do
  fromMaybe 0 <<< readTotal <$> query res sql
  where
  sql :: SQL
  sql = "SELECT COUNT(*) as total FROM {{path}}"

  readTotal :: JArray -> Maybe Int
  readTotal = I.fromNumber <=< toNumber <=< lookup "total" <=< toObject <=< head


port :: forall e. Resource -> Resource -> SQL ->
        StrMap VarMapValue ->
        Aff (RetryEffects (ajax :: AJAX | e)) JObject
port res dest sql vars = do
  guard $ isFile dest
  result <- slamjax $ defaultRequest
            { method = POST
            , headers = [ RequestHeader "Destination" $ resourcePath dest
                        , ContentType ldJSON
                        ]
            , url = printPath
                    $ queryUrl
                    </> rootify (resourceDir res)
                    </> dir (resourceName res)
                    </> file queryVars
            , content = Just (templated res sql)
            }

  if not $ succeeded result.status
    then throwError $ error $ readErr result.response
    else
    -- We expect result message to be valid json.
    either (throwError <<< error) pure
    $ jsonParser result.response >>= decodeJson
  where
  readErr :: String -> String
  readErr input =
    case jsonParser input >>= decodeJson >>= (.? "error") of
      -- All response is error text
      Left _ -> input
      -- Error is hided in json message, return only `error` field
      Right err -> err
  -- TODO: This should be somewhere better.
  queryVars :: String
  queryVars = maybe "" makeQueryVars <<< uncons $ toList vars

  pair :: Tuple String VarMapValue -> String
  pair (Tuple a b) = a <> "=" <> b

  makeQueryVars { head = h, tail = t } =
    foldl (\a v -> a <> "&" <> pair v) ("?" <> pair h) t

readError :: forall a. String -> String -> Either String a
readError msg input =
  let responseError = jsonParser input >>= decodeJson >>= (.? "error")
  in either (const $ Left msg) Left responseError

sample' :: forall e. Resource -> Maybe Int -> Maybe Int -> Aff (RetryEffects (ajax :: AJAX | e)) JArray
sample' res mbOffset mbLimit =
  if not $ isFile res
  then pure []
  else extractJArray <$> (getResponse msg $ retryGet uri)
  where
  msg = "error getting resource sample"
  uri = dataUrl
        </> rootify (resourceDir res)
        </> file ((resourceName res) <>
                  (maybe "" (("?offset=" <>) <<< show) mbOffset) <>
                  (maybe "" (("&limit=" <>) <<< show ) mbLimit))


sample :: forall e. Resource -> Int -> Int -> Aff (RetryEffects (ajax :: AJAX | e)) JArray
sample res offset limit = sample' res (Just offset) (Just limit)

all :: forall e. Resource -> Aff (RetryEffects (ajax :: AJAX | e)) JArray
all res = sample' res Nothing Nothing

fields :: forall e. Resource -> Aff (RetryEffects (ajax :: AJAX | e)) (Array String)
fields res = do
  jarr <- sample res 0 100
  case jarr of
    [] -> throwError $ error "empty file"
    _ -> pure $ nub $ concat (getFields <$> jarr)

mkURI :: Resource -> SQL -> FilePath
mkURI res sql =
  queryUrl
  </> file ("?q=" <> encodeURIComponent (templated res sql))

mkURI' :: Resource -> SQL -> FilePath
mkURI' res sql =
  queryUrl
  </> rootify (resourceDir res)
  </> dir (resourceName res)
  </> file ("?q=" <> encodeURIComponent (templated res sql))


templated :: Resource -> SQL -> SQL
templated res = replace "{{path}}" ("\"" <> resourcePath res <> "\"")

extractJArray :: String -> JArray
extractJArray =
  (foldl folder []) <<< (jsonParser <$>) <<< (split "\n")
  where
  folder :: JArray -> Either _ Json -> JArray
  folder agg (Right j) = agg ++ [j]
  folder agg _ = agg

getFields :: Json -> Array String
getFields json =
  filter (/= "") $ nub $ getFields' [] json

getFields' :: Array String -> Json -> Array String
getFields' [] json = getFields' [""] json
getFields' acc json =
  if isObject json
  then maybe acc (goObj acc) $ toObject json
  else if isArray json
       then maybe acc (goArr acc) $ toArray json
       else acc

goArr :: Array String -> JArray -> Array String
goArr acc arr =
  concat $ (getFields' ((<> "[*]") <$> acc) <$> arr)

goObj :: Array String -> JObject -> Array String
goObj acc obj = concat $ (goTuple acc <$> (fromList $ toList obj))

goTuple :: Array String -> Tuple String Json -> Array String
goTuple acc (Tuple key json) = getFields' ((\x -> x <> ".\"" <> key <> "\"") <$> acc) json
