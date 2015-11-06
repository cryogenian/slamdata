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

module Utils where

import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Global (readFloat, isNaN, readInt)

stringToInt :: String -> Maybe Int
stringToInt s =
  let n = readInt 10 s in
  if isNaN n
  then Nothing
  else fromNumber n

stringToNumber :: String -> Maybe Number
stringToNumber s =
  let n = readFloat s in
  if isNaN n
  then Nothing
  else Just n
