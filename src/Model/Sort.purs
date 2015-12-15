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

module Model.Sort where

import Prelude
import Data.Either (Either(..))

data Sort = Asc | Desc

notSort :: Sort -> Sort
notSort Asc = Desc
notSort _ = Asc

sort2string :: Sort -> String
sort2string Asc = "asc"
sort2string Desc = "desc"

string2sort :: String -> Either String Sort
string2sort "asc" = Right Asc
string2sort "desc" = Right Desc
string2sort _ = Left "incorrect sort string"

instance eqSort :: Eq Sort where
  eq Asc Asc = true
  eq Desc Desc = true
  eq _ _ = false
