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

module FileSystem.Routing.Search
       ( isSearchQuery
       , searchPath
       , filterByQuery
       ) where
import Prelude

import Data.Foldable (foldMap)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..), runFirst)
import Data.Minimatch as MM
import Data.String as Str
import Data.String.Regex as Rgx

import Model.Resource as M
import Text.SlamSearch as S
import Text.SlamSearch.Types as S

isSearchQuery :: S.SearchQuery -> Boolean
isSearchQuery query =
  not $ S.check unit query (\_ -> isNotSearchTerm)
  where isNotSearchTerm :: S.Term -> Boolean
        isNotSearchTerm (S.Term {predicate: p, labels: ls, include: i}) =
          case ls of
            Cons (S.Common "path") Nil -> true
            _ -> false

-- | check if string satisfies predicate from `purescript-search`
check :: S.Predicate -> String -> Boolean
check p prj =
  case p of
    S.Contains (S.Text str) -> match $ "*" <> escapeGlob str <> "*"
    S.Gt (S.Text str) -> compare str == GT
    S.Gte (S.Text str) -> compare str == GT || compare str == EQ
    S.Lt (S.Text str) -> compare str == LT
    S.Lte (S.Text str) -> compare str == LT || compare str == EQ
    S.Ne (S.Text str) -> compare str == LT || compare str == GT
    S.Eq (S.Text str) -> compare str == EQ
    -- since we use _minimatch_ to check `Contains` predicate
    -- `Like` predicate works exactly like `Contains` if we
    -- replace `% -> *` and `_ -> ?`
    S.Like s -> match $ like2glob s
    S.Range val val' ->
      let c = flip check prj in
      (c (S.Gte val) && c (S.Lte val')) ||
      (c (S.Lte val) && c (S.Gte val'))
    _ -> true
  where escapeGlob str = Str.replace "*" "\\*" $ Str.replace "?" "\\?" str
        percentRgx = Rgx.regex "%" flags
        underscoreRgx = Rgx.regex "_" flags
        flags = Rgx.noFlags{global = true}
        match a = MM.minimatch (Str.toLower a) (Str.toLower prj)
        compare = Str.localeCompare prj
        like2glob str =
          Rgx.replace percentRgx "*" $ Rgx.replace underscoreRgx "?" $ str

-- | Extract path predicate from search query
searchPath :: S.SearchQuery -> Maybe String
searchPath query =
  runFirst $ foldMap fn query
  where
  fn term = First $ case term of
    S.Term { include: true
           , predicate: S.Contains (S.Text path)
           , labels: Cons (S.Common "path") Nil } -> Just path
    _ -> Nothing




-- | Filtering function for items and predicates
filterByTerm :: M.Resource -> S.Term -> Boolean
filterByTerm r
  (S.Term {predicate: predicate, labels: labels, include: include}) =
  let name :: String
      name = M.resourceName r

      res :: String
      res = M.resourceTag r

      mbNot :: Boolean -> Boolean
      mbNot = if include then id else not

      check' :: String -> Boolean
      check' str = mbNot $ check predicate str
  in
  case labels of
    -- no labels -> check by both fields
    Nil -> check' name
    -- we've already checked _path_ when had got it from backend
    Cons (S.Common "path") Nil -> true
    -- check _name_
    Cons (S.Common "name") Nil -> check' name
    -- check _type_
    Cons (S.Common "type") Nil -> check' res
    -- check _type_
    Cons (S.Common "resource") Nil -> check' res
    _ -> false

-- | Filter by full search query
filterByQuery :: S.SearchQuery ->  M.Resource -> Boolean
filterByQuery query res =
  S.check res query filterByTerm
