{-
Copyright 2016 SlamData, Inc.

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

module SlamData.AuthRedirect.RedirectHashPayload
  ( RedirectHashPayload
  , uriHashParser
  , parseUriHash
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.String as S
import Data.StrMap as SM

import Text.Parsing.StringParser as P
import Text.Parsing.StringParser.Combinators as PC
import Text.Parsing.StringParser.String as PS

import OIDC.Crypt.Types as OIDC

type RedirectHashPayload =
  { idToken :: OIDC.IdToken
  , authUser :: Maybe String
  , prompt :: Maybe String
  }

parameterParser :: P.Parser (Tuple String String)
parameterParser = do
  key <- PC.many (PS.noneOf ['=']) <#> A.fromFoldable >>> S.fromCharArray
  PS.char '='
  val <- PC.many (PS.noneOf ['&']) <#> A.fromFoldable >>> S.fromCharArray
  pure $ Tuple key val

parametersParser :: P.Parser (SM.StrMap String)
parametersParser =
  PC.sepBy parameterParser (PS.char '&')
    <#> SM.fromFoldable

-- | Parse the payload from the URI hash. Example:
-- |
-- |     #id_token=foo&authuser=0&hd=slamdata.com&prompt=consent
-- |
uriHashParser :: P.Parser RedirectHashPayload
uriHashParser = do
  PS.char '#'
  params <- parametersParser
  let
    authUser = SM.lookup "authuser" params
    prompt = SM.lookup "prompt" params

  let lookup key = SM.lookup key params # maybe (P.fail $ "missing " <> key) pure
  idToken <- lookup "id_token" <#> OIDC.IdToken

  pure
    { idToken
    , authUser
    , prompt
    }

parseUriHash
  :: String
  -> Either P.ParseError RedirectHashPayload
parseUriHash =
  P.runParser uriHashParser
