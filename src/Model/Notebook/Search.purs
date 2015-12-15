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

module Model.Notebook.Search
  ( queryToSQL
  ) where

import Prelude

import Control.Bind (join)

import Data.Array (filter, catMaybes, head, nub)
import Data.Foldable
import Data.List (fromList, List())
import Data.Maybe
import Data.Semiring.Free
import Data.String (joinWith, indexOf)
import Data.String.Regex as RX

import Text.SlamSearch.Types as SS

queryToSQL
  :: Array String
  -> SS.SearchQuery
  -> String
queryToSQL fields query =
     "SELECT"
  <> (if needDistinct whereClause then " DISTINCT " else " ")
  <> topFields
  <> " from {{path}} where " <> whereOrFalse
  where
    topFields :: String
    topFields =
        joinWith ", "
      $ nub
      $ map (RX.replace firstDot "")
      $ catMaybes
      $ map join
      $ map head
      $ catMaybes
      $ map (RX.match topFieldRegex)
      $ fields

    topFieldRegex :: RX.Regex
    topFieldRegex = RX.regex "^\\.[^\\.\\[]+|^\\[.+\\]/" RX.noFlags

    whereOrFalse :: String
    whereOrFalse = if whereClause == "()" then "FALSE" else whereClause

    whereClause :: String
    whereClause =
        joinWith " OR "
      $ map oneWhereInput
      $ fromList
      $ runFree
      $ map (termToSQL fields) query

    oneWhereInput :: List String -> String
    oneWhereInput s = pars $ joinWith " AND " $ fromList s

needDistinct :: String -> Boolean
needDistinct input =
  (isJust $ indexOf "{*}" input) || (isJust $ indexOf "[*]" input)

termToSQL
  :: Array String
  -> SS.Term
  -> String
termToSQL fields (SS.Term {include: include, predicate: p, labels: ls}) =
  if not include
    then "NOT " <> pars (termToSQL fields $ SS.Term {include: true, predicate: p, labels: ls})
    else renderPredicate p $ labelsProjection fields (fromList ls)
  where
    renderPredicate :: SS.Predicate -> Array String -> String
    renderPredicate p prj =
      joinWith " OR " (predicateToSQL p <$> prj)

predicateToSQL
  :: SS.Predicate
  -> String
  -> String
predicateToSQL (SS.Contains (SS.Text v)) s =
  joinWith " OR " $
    [s <> " ~* '" <> (globToRegex $ containsToGlob v) <> "'"]
    <> (if needUnq v then render' v else [])
    <> (if not (needDateTime v) && needDate v then render date else [ ])
    <> (if needTime v then render time  else [])
    <> (if needDateTime v then render ts else [])
    <> (if needInterval v then render i else [])

  where
    containsToGlob :: String -> String
    containsToGlob v =
      if hasSpecialChars v
      then v
      else "*" <> v <> "*"

    hasSpecialChars :: String -> Boolean
    hasSpecialChars v =
      isJust (indexOf "*" v) || isJust (indexOf "?" v)

    quoted = quote v
    date = dated quoted
    time = timed quoted
    ts = datetimed quoted
    i = intervaled quoted
    render' v = [ "LOWER(" <> s <> ") = " <> v]
    render v = [s <> " = " <> v ]

predicateToSQL (SS.Range (SS.Text v) (SS.Text v')) s =
  joinWith " OR " $
    [ forR' quoted quoted' ]
    <> (if needUnq v && needUnq v' then [ forR v v' ] else [ ])
    <> (if needDate v && needDate v' then [ forR date date' ] else [ ])

  where
    quoted = quote v
    quoted' = quote v'

    date = dated quoted
    date' = dated quoted'

    forR' :: String -> String -> String
    forR' v v' =
      fold ["(LOWER(", s, ") >=", v, " AND LOWER(", s, ") <= ", v', ")"]

    forR :: String -> String -> String
    forR v v' =
      fold ["(", s, " >= ", v, " AND ", s, " <= ", v', ")"]

predicateToSQL (SS.Range (SS.Tag val) val') s =
  predicateToSQL (SS.Range (SS.Text val) val') s
predicateToSQL (SS.Range val (SS.Tag val')) s =
  predicateToSQL (SS.Range val (SS.Text val')) s
predicateToSQL (SS.Contains (SS.Tag v)) s =
  predicateToSQL (SS.Contains (SS.Text v)) s
predicateToSQL (SS.Eq v) s = qUnQ s "=" v
predicateToSQL (SS.Gt v) s = qUnQ s ">" v
predicateToSQL (SS.Gte v) s = qUnQ s ">=" v
predicateToSQL (SS.Lt v) s = qUnQ s "<" v
predicateToSQL (SS.Lte v) s = qUnQ s "<=" v
predicateToSQL (SS.Ne v) s = qUnQ s "<>" v
predicateToSQL (SS.Like v) s = s <> " ~* '" <> v <> "'"

globToRegex :: String -> String
globToRegex =
  (\x -> "^" <> x <> "$")
    <<< RX.replace askRegex "."
    <<< RX.replace starRegex ".*"
    <<< RX.replace globEscapeRegex "\\$&"
  where
    globEscapeRegex =
      RX.regex
        "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|]"
        RX.noFlags { global = true }

    starRegex = RX.regex "\\*" RX.noFlags { global = true }
    askRegex = RX.regex "\\?" RX.noFlags { global = true }

qUnQ
  :: String
  -> String
  -> SS.Value
  -> String
qUnQ s op v = pars $
  joinWith " OR " $
    [ forV' quoted ]
    <> (if needUnq unquoted then [ forV unquoted ] else [])
    <> (if not (needDateTime unquoted) && needDate unquoted then [ forV date ] else [])
    <> (if needTime unquoted then [ forV time ] else [])
    <> (if needDateTime unquoted then [ forV ts ] else [])
    <> (if needInterval unquoted then [ forV i ] else [])
  where
    unquoted = valueToSQL v
    quoted = quote unquoted
    date = dated quoted
    time = timed quoted
    ts = datetimed quoted
    i = intervaled quoted

    forV' v = fold ["LOWER(", s, ") ", op, " ", v]
    forV v = fold [s, " ", op, " ", v]

needUnq :: String -> Boolean
needUnq s =
  fromMaybe false ((show >>> (== s)) <$> Utils.stringToInt s)
  || fromMaybe false ((show >>> (== s)) <$> Utils.stringToNumber s)
  || s == "true"
  || s == "false"


needDate :: String -> Boolean
needDate = RX.test dateRegex
  where
    dateRegex =
      RX.regex
        """^(((19|20)([2468][048]|[13579][26]|0[48])|2000)[-]02[-]29|((19|20)[0-9]{2}[-](0[4678]|1[02])[-](0[1-9]|[12][0-9]|30)|(19|20)[0-9]{2}[-](0[1359]|11)[-](0[1-9]|[12][0-9]|3[01])|(19|20)[0-9]{2}[-]02[-](0[1-9]|1[0-9]|2[0-8])))$"""
        RX.noFlags


needTime :: String -> Boolean
needTime = RX.test timeRegex
  where
    timeRegex =
      RX.regex
        "^([0-1]?[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$"
        RX.noFlags


needDateTime :: String -> Boolean
needDateTime = RX.test dtRegex
  where
    dtRegex =
      RX.regex
        "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9]) (2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"
        RX.noFlags

needInterval :: String -> Boolean
needInterval = RX.test intervalRegex
  where
    intervalRegex =
      RX.regex
        "P((([0-9]*\\.?[0-9]*)Y)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)W)?(([0-9]*\\.?[0-9]*)D)?)?(T(([0-9]*\\.?[0-9]*)H)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)S)?)?"
        RX.noFlags

valueToSQL :: SS.Value -> String
valueToSQL v =
  case v of
    SS.Text v -> v
    SS.Tag v -> v

labelsProjection :: Array String -> Array SS.Label -> Array String
labelsProjection fields ls =
  nub
  $ RX.replace arrFieldRgx "[*]"
  <$> RX.replace firstDot ""
  <$> filter (RX.test $ labelsRegex ls) fields
  where
    arrFieldRgx :: RX.Regex
    arrFieldRgx = RX.regex "\\[\\d+\\]" RX.noFlags{global = true}

labelsRegex :: Array SS.Label -> RX.Regex
labelsRegex [] = RX.regex ".*" RX.noFlags
labelsRegex ls =
  RX.regex ("^" <> (foldMap mapFn ls) <> "$") RX.noFlags{ignoreCase = true}
  where
  mapFn :: SS.Label -> String
  mapFn (SS.Meta l) = mapFn (SS.Common l)
  mapFn (SS.Common "{*}") = "(\\.[^\\.]+)"
  mapFn (SS.Common "[*]") = "(\\[\\d+\\])"
  mapFn (SS.Common "*") = "(\\.[^\\.]+|\\[\\d+\\])"
  mapFn (SS.Common l)
    | RX.test (RX.regex "\\[\\d+\\]" RX.noFlags) l =
        RX.replace openSquare "\\["
      $ RX.replace closeSquare "\\]"
      $ l
    | otherwise = "(\\.\"" <> l <> "\"|\\." <> l <> ")"

  openSquare :: RX.Regex
  openSquare = RX.regex "\\[" RX.noFlags

  closeSquare :: RX.Regex
  closeSquare = RX.regex "\\]" RX.noFlags

firstDot :: RX.Regex
firstDot = RX.regex "^\\." RX.noFlags

quote :: String -> String
quote s = "'" <> s <> "'"

pars :: String -> String
pars s = "(" <> s <> ")"

dated :: String -> String
dated s = "DATE " <> s

timed :: String -> String
timed s = "TIME " <> s

datetimed :: String -> String
datetimed s = "TIMESTAMP " <> s

intervaled :: String -> String
intervaled s = "INTERVAL " <> s
