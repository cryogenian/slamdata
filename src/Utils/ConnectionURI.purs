-- TODO: eliminate the use of this entirely in favour of purescript-uri
module Utils.ConnectionURI
  ( URIParams()
  , CredentialParams()
  , HostParams()
  , PropParams()
  , toURI
  ) where

import Prelude
import Data.Array (null)
import Data.Maybe (Maybe(..), isJust, maybe, fromMaybe)
import qualified Data.String as S
import qualified Data.String.Regex as Rx

type URIParams =
  { path :: Maybe String
  , credentials :: Maybe CredentialParams
  , hosts :: Array HostParams
  , props :: Array PropParams
  }

type CredentialParams = { user :: String, password :: String }
type HostParams = { host :: String, port :: Maybe String }
type PropParams = { name :: String, value :: String }

toURI :: URIParams -> String
toURI params = "mongodb://"
  ++ maybe "" credentialsToURI params.credentials
  ++ (S.joinWith "," $ hostToURI <$> params.hosts)
  ++ (if isJust params.path || not (null params.props) then "/" else "")
  ++ (maybe "" fixSlashes params.path)
  ++ if null params.props
     then ""
     else "?" ++ (S.joinWith "&" $ propToURI <$> params.props)
  where
  credentialsToURI :: CredentialParams -> String
  credentialsToURI cred = cred.user ++ ":" ++ cred.password ++ "@"
  hostToURI :: HostParams -> String
  hostToURI host = host.host ++ (maybe "" (":" ++) host.port)
  propToURI :: PropParams -> String
  propToURI prop = prop.name ++ "=" ++ prop.value
  fixSlashes :: String -> String
  fixSlashes s | S.take 1 s == "/" = fixSlashes (S.drop 1 s)
               | otherwise = Rx.replace (Rx.regex "/{2,}" Rx.noFlags { global = true }) "/" s
