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

module SlamData.FileSystem.Routing.Search
  ( isSearchQuery
  , searchPath
  , filterByQuery
  ) where

import SlamData.Prelude

import Data.List (List(..))
import Data.Maybe.First (First(..), runFirst)
import Data.Minimatch as MM
import Data.String as Str
import Data.String.Regex as Rgx

import SlamData.FileSystem.Resource as M

import Text.SlamSearch as SS
import Text.SlamSearch.Types as SST

isSearchQuery :: SST.SearchQuery -> Boolean
isSearchQuery query =
  not $ SS.check unit query (\_ -> isNotSearchTerm)
  where
  isNotSearchTerm :: SST.Term -> Boolean
  isNotSearchTerm (SST.Term {predicate: p, labels: ls, include: i}) =
    case ls of
      Cons (SST.Common "path") Nil -> true
      _ -> false

-- | check if string satisfies predicate from `purescript-search`
check :: SST.Predicate -> String -> Boolean
check p prj =
  case p of
    SST.Contains (SST.Text str) -> match $ "*" <> escapeGlob str <> "*"
    SST.Gt (SST.Text str) -> compare str == GT
    SST.Gte (SST.Text str) -> compare str == GT || compare str == EQ
    SST.Lt (SST.Text str) -> compare str == LT
    SST.Lte (SST.Text str) -> compare str == LT || compare str == EQ
    SST.Ne (SST.Text str) -> compare str == LT || compare str == GT
    SST.Eq (SST.Text str) -> compare str == EQ
    -- since we use _minimatch_ to check `Contains` predicate
    -- `Like` predicate works exactly like `Contains` if we
    -- replace `% -> *` and `_ -> ?`
    SST.Like s -> match $ like2glob s
    SST.Range val val' ->
      let c = flip check prj in
      (c (SST.Gte val) && c (SST.Lte val')) ||
      (c (SST.Lte val) && c (SST.Gte val'))
    _ -> true
  where
  escapeGlob str = Str.replace "*" "\\*" $ Str.replace "?" "\\?" str
  percentRgx = unsafePartial fromRight $ Rgx.regex "%" flags
  underscoreRgx = unsafePartial fromRight $ Rgx.regex "_" flags
  flags = Rgx.noFlags{global = true}
  match a = MM.minimatch (Str.toLower a) (Str.toLower prj)
  compare = Str.localeCompare prj
  like2glob str =
    Rgx.replace percentRgx "*" $ Rgx.replace underscoreRgx "?" $ str

-- | Extract path predicate from search query
searchPath :: SST.SearchQuery -> Maybe String
searchPath query =
  runFirst $ foldMap fn query
  where
  fn term = First $ case term of
    SST.Term { include: true
           , predicate: SST.Contains (SST.Text path)
           , labels: Cons (SST.Common "path") Nil } -> Just path
    _ -> Nothing




-- | Filtering function for items and predicates
filterByTerm :: M.Resource -> SST.Term -> Boolean
filterByTerm r
  (SST.Term {predicate: predicate, labels: labels, include: include}) =
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
    Cons (SST.Common "path") Nil -> true
    -- check _name_
    Cons (SST.Common "name") Nil -> check' name
    -- check _type_
    Cons (SST.Common "type") Nil -> check' res
    -- check _type_
    Cons (SST.Common "resource") Nil -> check' res
    _ -> false

-- | Filter by full search query
filterByQuery :: SST.SearchQuery ->  M.Resource -> Boolean
filterByQuery query res =
  SS.check res query filterByTerm
