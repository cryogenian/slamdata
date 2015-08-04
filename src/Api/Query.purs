module Api.Query (query, port, sample, SQL(), fields, count, all, templated) where

import Prelude
import Api.Common (getResponse, succeeded, retryGet)
import Config (queryUrl, dataUrl)
import Control.Apply (lift2)
import Control.Bind ((<=<), (>=>))
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (JArray(), Json(), JObject(), jsonEmptyObject, isArray, isObject, foldJson, toArray, toObject, toNumber, fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concat, nub, filter, head)
import Utils (encodeURIComponent)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Foldable (foldl)
import Data.List (uncons, fromList)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (split, replace)
import Data.StrMap (StrMap(), keys, toList, empty, lookup)
import Data.Tuple (Tuple(..))
import Model.Notebook.Port (VarMapValue())
import Model.Path (AnyPath())
import Model.Resource (Resource(), resourcePath, isFile, _name)
import Network.HTTP.Affjax (Affjax(), AJAX(), affjax, defaultRequest)
import Network.HTTP.Method (Method(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Optic.Core 
import qualified Data.Int as I


-- | This is template string where actual path is encoded like {{path}}
type SQL = String

query :: forall e. Resource -> SQL -> Aff (ajax :: AJAX, avar :: AVAR | e) JArray
query res sql =
  if not $ isFile res
  then pure []
  else extractJArray <$> (getResponse msg $ retryGet uri)
  where
  msg = "error in query"
  uri = mkURI res sql

count :: forall e. Resource -> Aff (ajax :: AJAX | e) Int
count res = do
  fromMaybe 0 <<< readTotal <$> query res sql
  where
  sql :: SQL
  sql = "SELECT COUNT(*) as total FROM {{path}}"

  readTotal :: JArray -> Maybe Int
  readTotal = I.fromNumber <=< toNumber <=< lookup "total" <=< toObject <=< head


port :: forall e. Resource -> Resource -> SQL ->
        StrMap VarMapValue ->
        Aff (ajax :: AJAX | e) JObject
port res dest sql vars =
  if not (isFile dest)
  then pure empty
  else do
    result <- retry Nothing affjax $ defaultRequest
            { method = POST
            , headers = [RequestHeader "Destination" $ resourcePath dest]
            , url = queryUrl <> resourcePath res <> queryVars
            , content = Just (templated res sql)
            }

    either (throwError <<< error) pure $ content result.response
  where
  swap :: forall a b. Either a b -> Either b a
  swap (Right a) = Left a
  swap (Left a) = Right a

  -- TODO: This should be somewhere better.
  queryVars :: String
  queryVars = maybe "" makeQueryVars <<< uncons $ toList vars

  -- TODO: Need to encode query component.
  pair :: Tuple String VarMapValue -> String
  pair (Tuple a b) = a <> "=" <> b

  makeQueryVars { head = h, tail = t } = 
    foldl (\a v -> a <> "&" <> pair v) ("?" <> pair h) t

  content :: String -> Either String JObject
  content input = do
    json <- jsonParser input >>= decodeJson
    swap $ json .? "error"
    pure json

sample' :: forall e. Resource -> Maybe Int -> Maybe Int -> Aff (ajax :: AJAX, avar :: AVAR | e) JArray
sample' res mbOffset mbLimit =
  if not $ isFile res
  then pure []
  else extractJArray <$> (getResponse msg $ retryGet uri)
  where
  msg = "error getting resource sample"
  uri = dataUrl <> resourcePath res <>
        (maybe "" (("?offset=" <>) <<< show) mbOffset) <>
        (maybe "" (("&limit=" <>) <<< show ) mbLimit)

sample :: forall e. Resource -> Int -> Int -> Aff (ajax :: AJAX | e) JArray
sample res offset limit = sample' res (Just offset) (Just limit)

all :: forall e. Resource -> Aff (ajax :: AJAX | e) JArray
all res = sample' res Nothing Nothing

fields :: forall e. Resource -> Aff (ajax :: AJAX | e) (Array String)
fields res = do
  jarr <- sample res 0 100
  case jarr of
    [] -> throwError $ error "empty file"
    _ -> pure $ nub $ concat (getFields <$> jarr)


mkURI :: Resource -> SQL -> String
mkURI res sql =
  queryUrl <> resourcePath res <> "?q=" <> encodeURIComponent (templated res sql)

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
