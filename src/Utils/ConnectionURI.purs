-- TODO: eliminate the use of this entirely in favour of purescript-uri
module Utils.ConnectionURI
  ( URIParams()
  , CredentialParams()
  , HostParams()
  , PropParams()
  , toURI
  ) where

import Data.Array (null)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (joinWith)

type URIParams =
  { name :: Maybe String
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
  ++ (joinWith "," $ hostToURI <$> params.hosts)
  ++ "/" ++ (fromMaybe "" params.name)
  ++ if null params.props
     then ""
     else "?" ++ (joinWith "&" $ propToURI <$> params.props)
  where
  credentialsToURI :: CredentialParams -> String
  credentialsToURI cred = cred.user ++ ":" ++ cred.password ++ "@"
  hostToURI :: HostParams -> String
  hostToURI host = host.host ++ (maybe "" (":" ++) host.port)
  propToURI :: PropParams -> String
  propToURI prop = prop.name ++ "=" ++ prop.value
