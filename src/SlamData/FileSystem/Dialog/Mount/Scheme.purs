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

module SlamData.FileSystem.Dialog.Mount.Scheme where

import Prelude

import Data.Maybe (Maybe(..))

data Scheme = MongoDB | SQL2

instance eqScheme :: Eq Scheme where
  eq MongoDB MongoDB = true
  eq SQL2 SQL2 = true
  eq _ _ = false

instance ordScheme :: Ord Scheme where
  compare MongoDB MongoDB = EQ
  compare MongoDB SQL2 = LT
  compare SQL2 SQL2 = EQ
  compare SQL2 MongoDB = GT

schemeToString :: Scheme -> String
schemeToString MongoDB = "MongoDB"
schemeToString SQL2 = "SQL²"

schemeFromString :: String -> Maybe Scheme
schemeFromString "MongoDB" = Just MongoDB
schemeFromString "SQL²" = Just SQL2
schemeFromString _ = Nothing

schemes :: Array Scheme
schemes = [MongoDB, SQL2]
