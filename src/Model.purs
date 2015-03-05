-- | Common stuff to use in many components
module Model where

import Data.Maybe
import Data.Either

import Data.Argonaut.Decode
import Data.Argonaut.Combinators

data Sort = Asc | Desc

sortToString :: Sort -> String
sortToString Asc = "asc"
sortToString Desc = "desc"

sortFromString :: String -> Maybe Sort
sortFromString "asc" = Just Asc
sortFromString "desc" = Just Desc
sortFromString _ = Nothing


data Mount =  File | Database | Notebook | Directory | Table 

instance decodeJsonMount :: DecodeJson Mount where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "file" -> return File
      "database" -> return Database
      "notebook" -> return Notebook
      "directory" -> return Directory
      "table" -> return Table
      _ -> Left "incorrect string in decoding Mount type"
