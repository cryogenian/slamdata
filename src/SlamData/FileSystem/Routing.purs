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

module SlamData.FileSystem.Routing
 ( Routes(..)
 , routing
 , browseURL
 , parentURL
 ) where

import SlamData.Prelude

import Data.Path.Pathy as P

import Global as Global

import Routing.Match (Match, eitherMatch)
import Routing.Match.Class (param)

import SlamData.Config as Config
import SlamData.Common.Sort (Sort(..), string2sort, sort2string)
import SlamData.FileSystem.Routing.Salt (Salt(..), runSalt)

import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Types (SearchQuery)

import Utils.Path as UP

data Routes
  = Salted Sort SearchQuery Salt
  | SortAndQ Sort SearchQuery
  | Sort Sort
  | Index

routing :: Match Routes
routing = salted <|> bothRoute <|> oneRoute <|> index
  where
  salted = Salted <$> sort <*> query <*> salt
  bothRoute = SortAndQ <$> sort <*> query
  oneRoute = Sort <$> sort
  index = pure Index
  sort = eitherMatch (string2sort <$> param "sort")
  query = eitherMatch (mkQuery <$> param "q")
  salt = eitherMatch (getSalt <$> param "salt")

getSalt :: String -> Either String Salt
getSalt input =
  if input /= ""
  then Right $ Salt input
  else Left "incorrect salt"

browseURL :: Maybe String -> Sort -> Salt -> UP.DirPath -> String
browseURL search sort salt path =
  Config.browserUrl
    <> "#?q=" <> q
    <> "&sort=" <> sort2string sort
    <> "&salt=" <> runSalt salt

  where
  search' =
    fromMaybe "" search # \s ->
      if s == "" then s else s <> " "

  q =
    Global.encodeURIComponent $
      search'
        <> "path:\""
        <> P.printPath path
        <> "\""

parentURL :: UP.AnyPath -> String
parentURL childPath =
  browseURL Nothing Asc (Salt "") $ UP.getDir childPath
