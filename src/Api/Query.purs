module Api.Query (query, port, sample, SQL(), fields, count, all, templated) where

import Api.Common (getResponse, succeeded)
import Config (queryUrl, dataUrl)
import Control.Apply (lift2)
import Control.Bind ((<=<), (>=>))
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (JArray(), Json(), JObject(), jsonEmptyObject, isArray, isObject, foldJson, toArray, toObject, toNumber, fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concat, nub, filter, head)
import Data.DOM.Simple.Encode (encodeURIComponent)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (split, replace)
import Data.StrMap (StrMap(), keys, toList, empty, lookup)
import Data.Tuple (Tuple(..))
import Model.Notebook.Port (VarMapValue())
import Model.Path (AnyPath())
import Model.Resource (Resource(), resourcePath, isFile, _name, parent)
import Network.HTTP.Affjax (Affjax(), AJAX(), get, affjax, defaultRequest)
import Network.HTTP.Method (Method(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Optic.Core ((^.))
import qualified Data.Int as I


-- | This is template string where actual path is encoded like {{path}}
type SQL = String

query :: forall e. Resource -> SQL -> Aff (ajax :: AJAX | e) JArray
query res sql =
  if not $ isFile res
  then pure []
  else extractJArray <$> (getResponse msg $ get uri)
  where
  msg = "error in query"
  uri = mkURI res sql

count :: forall e. Resource -> Aff (ajax :: AJAX | e) Number
count res = do
  fromMaybe 0 <<< readTotal <$> query res sql
  where
  sql :: SQL
  sql = "SELECT COUNT(*) as total FROM {{path}}"

  readTotal :: JArray -> Maybe Number
  readTotal = toNumber <=< lookup "total" <=< toObject <=< head

foreign import unconsNative """
  function unconsNative (empty) {
    return function (next) {
      return function (xs) {
        return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
      };
    };
  }
  """ :: forall a b. (Unit -> b) ->
                     (a -> Array a -> b) ->
                     Array a ->
                     b

port :: forall e. Resource -> Resource -> SQL ->
        StrMap VarMapValue ->
        Aff (ajax :: AJAX | e) JObject
port res dest sql vars =
  if not (isFile dest)
  then pure empty
  else do
    result <- affjax $ defaultRequest
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

  uncons :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
  uncons = unconsNative (const Nothing) \x xs -> Just { head: x, tail: xs }

  content :: String -> Either String JObject
  content input = do
    json <- jsonParser input >>= decodeJson
    swap $ json .? "error"
    pure json

sample' :: forall e. Resource -> Maybe I.Int -> Maybe I.Int -> Aff (ajax :: AJAX |e) JArray
sample' res mbOffset mbLimit =
  if not $ isFile res
  then pure []
  else (readValue <$>) <$> (extractJArray <$> (getResponse msg $ get uri))
  where
  msg = "error getting resource sample"
  uri = dataUrl <> resourcePath res <>
        (maybe "" (("?offset=" <>) <<< show <<< I.toNumber) mbOffset) <>
        (maybe "" (("&limit=" <>) <<< show <<< I.toNumber) mbLimit)

sample :: forall e. Resource -> I.Int -> I.Int -> Aff (ajax :: AJAX | e) JArray
sample res offset limit = sample' res (Just offset) (Just limit)

all :: forall e. Resource -> Aff (ajax :: AJAX | e) JArray
all res = sample' res Nothing Nothing

fields :: forall e. Resource -> Aff (ajax :: AJAX | e) [String]
fields res = do
  jarr <- sample res (I.fromNumber 0) (I.fromNumber 100)
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
  folder agg (Right j) = j:agg
  folder agg _ = agg


getFields :: Json -> [String]
getFields json =
  filter (/= "") $ nub $ getFields' [] json


getFields' :: [String] -> Json -> [String]
getFields' [] json = getFields' [""] json
getFields' acc json =
  if isObject json
  then maybe acc (goObj acc) $ toObject json
  else if isArray json
       then maybe acc (goArr acc) $ toArray json
       else acc

goArr :: [String] -> JArray -> [String]
goArr acc arr =
  concat $ (getFields' ((<> "[*]") <$> acc) <$> arr)

goObj :: [String] -> JObject -> [String]
goObj acc obj = concat $ (goTuple acc <$> (toList obj))

goTuple :: [String] -> Tuple String Json -> [String]
goTuple acc (Tuple key json) = getFields' ((\x -> x <> ".\"" <> key <> "\"") <$> acc) json

-- temporary files are written to `value` field or even `value.value`
readValue :: Json -> Json
readValue json = fromObject $ fromRight do
  obj <- decodeJson json
  if keys obj == ["value"]
    then let value :: Either _ JObject
             value = obj .? "value" in
         if keys <$> value == pure ["value"]
         then (.? "value") >=> (.? "value") $ obj
         else value
    else pure obj
