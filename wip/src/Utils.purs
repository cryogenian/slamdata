module Utils where

import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Global (readFloat, isNaN, readInt)

s2i :: String -> Maybe Int
s2i s =
  let n = readInt 10 s in
  if isNaN n
  then Nothing
  else fromNumber n

s2n :: String -> Maybe Number
s2n s =
  let n = readFloat s in
  if isNaN n
  then Nothing
  else Just n
