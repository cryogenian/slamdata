-- | Common stuff to use in many components
module Model where

import Data.Maybe
import Data.Either

import Data.Argonaut.Decode
import Data.Argonaut.Combinators

import Text.SlamSearch.Parser
import Text.SlamSearch.Parser.Terms
import Text.SlamSearch.Parser.Tokens

import Prelude

data Sort = Asc | Desc

sortNot :: Sort -> Sort
sortNot Asc = Desc
sortNot Desc = Asc

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
      "mount" -> return Database
      "notebook" -> return Notebook
      "directory" -> return Directory
      "table" -> return Table
      _ -> Left "incorrect string in decoding Mount type"


filterTerm :: SearchQuery -> (SearchTerm -> Boolean) -> [SearchTerm]
filterTerm query fn =
  filterTerm' query []
  where filterTerm' EmptyQuery acc = acc
        filterTerm' (SearchAnd term query') acc
          | fn term = filterTerm' query' (term:acc)
          | otherwise = filterTerm' query' acc


extractSimpleTerm :: SearchTerm -> SearchTermSimple
extractSimpleTerm (IncludeTerm term) = term
extractSimpleTerm (ExcludeTerm term) = term 

getPathTerms :: SearchQuery -> [SearchTerm]
getPathTerms query =
  let filterFn term =
        case extractSimpleTerm term of
          SearchTermSimple [Common("path")] p -> true
          _ -> false 
  in filterTerm query filterFn

getPathTerm :: SearchQuery -> Maybe SearchTerm
getPathTerm query =
  let terms = getPathTerms query in
  case terms of
    [] -> Nothing
    x:xs -> Just x
            

getNotPathTerms :: SearchQuery -> [SearchTerm]
getNotPathTerms query =
  let filterFn term =
        case extractSimpleTerm term of
          SearchTermSimple [Common("path")] p -> false
          _ -> true
  in filterTerm query filterFn

