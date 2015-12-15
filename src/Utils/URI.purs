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

module Utils.URI
 ( URIParams()
 , CredentialParams()
 , HostParams()
 , PropParams()
 , toURI
 ) where

import Prelude
import Data.Array (null)
import Data.Maybe (Maybe(), isJust, maybe)
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
