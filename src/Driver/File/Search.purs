module Driver.File.Search (
  isSearchQuery,
  filterByQuery,
  searchPath
  ) where
import Data.Maybe
import Data.Monoid.First (First(..), runFirst)
import Data.Foldable (foldMap)
import qualified Text.SlamSearch as S
import qualified Text.SlamSearch.Printer as S
import qualified Text.SlamSearch.Types as S
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Data.Minimatch as MM
import qualified Model.Resource as M
import Optic.Core ((^.))

isSearchQuery :: S.SearchQuery -> Boolean
isSearchQuery query =
  not $ S.check unit query (\_ -> isNotSearchTerm)
  where isNotSearchTerm :: S.Term -> Boolean
        isNotSearchTerm (S.Term {predicate: p, labels: ls, include: i}) =
          case ls of
            [S.Common "path"] -> true
            _ -> false

-- | check if string satisfies predicate from `purescript-search`
check :: S.Predicate -> String -> Boolean
check p prj =
  case p of
    S.Contains (S.Text str) -> match $ "*" <> escapeGlob str <> "*"
    S.Gt (S.Text str) -> compare str > 0
    S.Gte (S.Text str) -> compare str >= 0
    S.Lt (S.Text str) -> compare str < 0
    S.Lte (S.Text str) -> compare str <= 0
    S.Ne (S.Text str) -> compare str /= 0
    S.Eq (S.Text str) -> compare str == 0
    -- since we use _minimatch_ to check `Contains` predicate
    -- `Like` predicate works exactly like `Contains` if we
    -- replace `% -> *` and `_ -> ?`
    S.Like s -> match $ like2glob s
    S.Contains (S.Range str str') ->
      let c = flip check prj in
      (c (S.Gte (S.Text str)) && c (S.Lte (S.Text str'))) ||
      (c (S.Lte (S.Text str)) && c (S.Gte (S.Text str')))
    -- filters only when it means something. S.Eq S.Range - meaningless
    -- searching by tag in this context has no meaning too
    _ -> true
  where escapeGlob str = Str.replace "*" "\\*" $ Str.replace "?" "\\?" str
        percentRgx = Rgx.regex "%" flags
        underscoreRgx = Rgx.regex "_" flags
        flags = Rgx.noFlags{global = true}
        match a = MM.minimatch (Str.toLower a) (Str.toLower prj)
        compare = Str.localeCompare prj
        like2glob str =
          Rgx.replace percentRgx "*" $ Rgx.replace underscoreRgx "?" $ str


-- | Filtering function for items and predicates
filterByTerm :: M.Resource -> S.Term -> Boolean
filterByTerm r
  (S.Term {predicate: predicate, labels: labels, include: include}) =
  let name :: String
      name = r ^. M.nameL

      res :: String
      res = M.resourceTag r

      mbNot :: Boolean -> Boolean
      mbNot = if include then id else not

      check' :: String -> Boolean
      check' str = mbNot $ check predicate str
  in
  case labels of
    -- no labels -> check by both fields
    [] -> check' name || check' res
    -- we've already checked _path_ when had got it from backend
    [S.Common "path"] -> true
    -- check _name_
    [S.Common "name"] -> check' name
    -- check _type_
    [S.Common "type"] -> check' res
    -- check _type_
    [S.Common "resource"] -> check' res
    _ -> false

-- | Filter by full search query
filterByQuery :: S.SearchQuery ->  M.Resource -> Boolean
filterByQuery query res =
  S.check res query filterByTerm

-- | Extract path predicate from search query
searchPath :: S.SearchQuery -> Maybe String
searchPath query =
  runFirst $ foldMap fn query
  where fn term = First case term of
          S.Term {include: true,
                  predicate: S.Contains (S.Text path),
                  labels: [S.Common "path"]} -> Just path
          _ -> Nothing
