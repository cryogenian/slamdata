module Model where

import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Monoid.All (All(..), runAll)
import Data.Foldable (fold)
import Data.String (toLower, indexOf, localeCompare)

import Data.Argonaut.Decode
import Data.Argonaut.Combinators

import Text.SlamSearch.Parser
import Text.SlamSearch.Parser.Terms
import Text.SlamSearch.Parser.Tokens
import Text.SlamSearch.Parser.Values

import Data.Minimatch

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

mountFromString :: String -> Either String Mount
mountFromString str = 
  case toLower str of
    "file" -> return File
    "mount" -> return Database
    "notebook" -> return Notebook
    "directory" -> return Directory
    "table" -> return Table
    _ -> Left "incorrect string in decoding Mount type"

mountToString :: Mount -> String
mountToString mount =
  case mount of
    File -> "file"
    Database -> "mount"
    Notebook -> "notebook"
    Directory -> "directory"
    Table -> "table"
    
    
instance eqMount :: Eq Mount where
  (==) File File = true
  (==) Database Database = true
  (==) Notebook Notebook = true
  (==) Directory Directory = true
  (==) Table Table = true
  (==) _ _ = false
  (/=) a b = not $ a == b

instance decodeJsonMount :: DecodeJson Mount where
  decodeJson json = do
    str <- decodeJson json
    mountFromString str


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
          SearchTermSimple [Common("path")] _ -> true
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
          SearchTermSimple [Common("path")] _ -> false
          _ -> true
  in filterTerm query filterFn


type ItemLogic = {
  resource :: Mount,
  name :: String
  }
-- To slamdata/purescript-search#5 mostly and
-- probably slamdata/purescript-search#6
queryToTerms :: SearchQuery -> [SearchTerm]
queryToTerms query =
  queryToTerms' query []
  where queryToTerms' query acc =
          case query of
            EmptyQuery -> acc
            SearchAnd term query' -> queryToTerms' query' (term:acc)

conformQuery :: SearchQuery -> ItemLogic -> Boolean
conformQuery query a = conformAnd a (queryToTerms query)

conformAnd :: ItemLogic -> [SearchTerm] -> Boolean
conformAnd a terms =
  runAll $ fold $ (All <<< conformTerm a) <$> terms 

conformTerm :: ItemLogic -> SearchTerm -> Boolean
conformTerm a (IncludeTerm simple) = conformT a simple
conformTerm a (ExcludeTerm simple) = not $ conformT a simple

conformT :: ItemLogic -> SearchTermSimple -> Boolean
conformT a (SearchTermSimple [] p) = overOr a p 
conformT a (SearchTermSimple [Common("path")] _) = true
conformT a (SearchTermSimple [Common("type")] p) = overType a p
conformT a (SearchTermSimple [Common("name")] p) = overName a p

overOr :: ItemLogic -> Predicate -> Boolean
overOr a p = overType a p || overName a p

overType :: ItemLogic -> Predicate -> Boolean
overType a p = check p $ mountToString a.resource

overName :: ItemLogic -> Predicate -> Boolean
overName a p = check p $ a.name

check :: Predicate -> String -> Boolean
check (ContainsPredicate (TextVal tst)) probe =
  indexOf tst probe /= -1
check (EqPredicate (TextVal tst)) probe =
  tst == probe
check (GtPredicate (TextVal tst)) probe = 
  localeCompare tst probe > 0
check (LtPredicate (TextVal tst)) probe =
  localeCompare tst probe < 0
check (GtePredicate (TextVal tst)) probe =
  localeCompare tst probe >= 0
check (LtePredicate (TextVal tst)) probe =
  localeCompare tst probe <= 0
check (NePredicate (TextVal tst)) probe =
  localeCompare tst probe /= 0
check (LikePredicate (TextVal tst)) probe =
  -- since it is not Glob, but TextVal
  tst == probe
check (ContainsPredicate (RangeVal start end)) probe =
  localeCompare start probe < 0 && localeCompare end probe > 0
check (EqPredicate (RangeVal start end)) probe =
  localeCompare start probe < 0 && localeCompare end probe > 0
check (GtPredicate (RangeVal start end)) probe =
  localeCompare start probe < 0 && localeCompare end probe < 0
check (GtePredicate (RangeVal start end)) probe =
  localeCompare start probe <= 0 && localeCompare end probe <= 0
check (LtPredicate (RangeVal start end)) probe =
  localeCompare start probe > 0 && localeCompare end probe > 0
check (LtePredicate (RangeVal start end)) probe =
  localeCompare start probe >= 0 && localeCompare end probe >= 0
check (NePredicate (RangeVal start end)) probe =
  localeCompare start probe > 0 && localeCompare end probe < 0
check (LikePredicate (RangeVal start end)) probe =
  check (EqPredicate (RangeVal start end)) probe

check (ContainsPredicate (Glob tst)) probe =
  check (ContainsPredicate (TextVal tst)) probe
check (EqPredicate (Glob tst)) probe = check (EqPredicate (TextVal tst)) probe
check (LtPredicate (Glob tst)) probe = check (LtPredicate (TextVal tst)) probe
check (GtPredicate (Glob tst)) probe = check (GtPredicate (TextVal tst)) probe
check (LtePredicate (Glob tst)) probe = check (LtePredicate (TextVal tst)) probe
check (GtePredicate (Glob tst)) probe = check (GtePredicate (TextVal tst)) probe
check (NePredicate (Glob tst)) probe = check (NePredicate (TextVal tst)) probe
check (LikePredicate (Glob tst)) probe = 
  minimatch tst probe
check _ probe = true

  




