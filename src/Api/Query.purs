module Api.Query (query, port, sample, SQL(), fields) where

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Network.HTTP.Affjax (Affjax(), AJAX(), get, affjax, defaultRequest)
import Network.HTTP.Method (Method(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Api.Common (getResponse, succeeded)
import Config (queryUrl, dataUrl)
import Model.Resource (Resource(), resourcePath, AnyPath(), isFile, _name, parent)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (JArray(), Json(), JObject(), jsonEmptyObject, isArray, isObject, foldJson, toArray, toObject)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Decode (decodeJson)
import Data.String (split, replace)
import Data.Foldable (foldl)
import Optic.Core ((.~))
import Data.DOM.Simple.Encode (encodeURIComponent)
import Data.Int (fromNumber, toNumber, Int())
import Data.StrMap (keys, toList, empty)
import Control.Apply (lift2)
import Data.Array (concat, nub, filter)
import Optic.Core ((^.))


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

port :: forall e. Resource -> Resource -> SQL ->
        Aff (ajax :: AJAX | e) JObject 
port res dest sql = 
  if not (isFile res && isFile dest)
  then pure empty
  else do 
    result <- affjax $ defaultRequest
            { method = POST
            , headers = [RequestHeader "Destination" $ resourcePath dest]
            , url = mkURI res sql
            }

    either (throwError <<< error) pure $ content result.response
  where
  msg :: String
  msg = "error in calling `port`"  

  swap :: forall a b. Either a b -> Either b a
  swap (Right a) = Left a
  swap (Left a) = Right a

  content :: String -> Either String JObject
  content input = do
    json <- jsonParser input >>= decodeJson
    swap $ json .? "error"
    pure json


sample :: forall e. Resource -> Int -> Int -> Aff (ajax :: AJAX | e) JArray 
sample res offset limit = 
  if not $ isFile res
  then pure []
  else extractJArray <$> (getResponse msg $ get uri)
  where
  msg = "error getting resource sample"
  uri = dataUrl <> resourcePath res <>
        "?offset=" <> show (toNumber offset) <> "&limit=" <> show (toNumber limit)

fields :: forall e. Resource -> Aff (ajax :: AJAX | e) [String]
fields res = do
  jarr <- sample res (fromNumber 0) (fromNumber 100)
  pure $ nub $ concat (getFields <$> jarr)



mkURI :: Resource -> SQL -> String
mkURI res sql = 
  queryUrl <> resourcePath res <> "?q=" <> encodeURIComponent templated
  where
  templated = replace "{{path}}" ("\"" <> resourcePath res <> "\"") sql

  
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
  concat $ [acc] <> (getFields' ((<> "[*]") <$> acc) <$> arr)

goObj :: [String] -> JObject -> [String]
goObj acc obj = concat $ [acc] <> (goTuple acc <$> (toList obj))

goTuple :: [String] -> Tuple String Json -> [String]
goTuple acc (Tuple key json) = getFields' ((\x -> x <> "." <> key) <$> acc) json 
    
