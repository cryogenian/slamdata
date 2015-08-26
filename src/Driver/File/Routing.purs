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

module Driver.File.Routing
  ( Routes(..)
  , routing
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Model.File.Salt (Salt(..))
import Model.File.Sort (Sort(..), string2sort)
import Routing.Match (Match(), eitherMatch)
import Routing.Match.Class (param)
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Types (SearchQuery())

data Routes
  = Salted Sort SearchQuery Salt
  | SortAndQ Sort SearchQuery
  | Sort Sort
  | Index

getSalt :: String -> Either String Salt
getSalt input =
  if input /= "" then Right (Salt input)
  else Left "incorrect salt"

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
