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
  , needFields
  ) where

import Prelude
import Control.Apply (lift2)
import Data.Either
import Data.List (fromList, List(..))
import Data.Maybe
import Data.Semiring.Free
import Data.Foldable
import Data.String (joinWith, indexOf)
import Data.String.Regex as RX
import Text.SlamSearch as SS
import Text.SlamSearch.Types as SS
import Global

queryToSQL
  :: Array String
  -> SS.SearchQuery
  -> String
queryToSQL fields query =
  "SELECT"
    <> (if needDistinct whereClause then " DISTINCT " else " ")
    <> "* from {{path}} where "
    <> whereClause
  where
    whereClause =
      joinWith " OR " $
        pars <$> joinWith " AND " <$> (fromList <<< (fromList <$>) $
          runFree $ (termToSQL fields) <$> query)

needDistinct :: String -> Boolean
needDistinct input = isJust $ indexOf "[*]" input

needFields :: SS.SearchQuery -> Boolean
needFields query =
  SS.check unit query needFields'
  where
    needFields' :: Unit -> SS.Term -> Boolean
    needFields' _ (SS.Term {labels: Nil}) = true
    needFields' _ _ = false

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
predicateToSQL (SS.Contains (SS.Range v v')) s = range v v' s
predicateToSQL (SS.Contains (SS.Text v)) s =
  joinWith " OR " $
    [s <> " ~* '" <> escapeRegex v <> "'"]
    <> (if needUnq v then render' v else [])
    <> (if not (needDateTime v) && needDate v then render date else [ ])
    <> (if needTime v then render time  else [])
    <> (if needDateTime v then render ts else [])
    <> (if needInterval v then render i else [])

  where
    quoted = quote v
    date = dated quoted
    time = timed quoted
    ts = datetimed quoted
    i = intervaled quoted
    render' v = [ "LOWER(" <> s <> ") = " <> v]
    render v = [s <> " = " <> v ]

predicateToSQL (SS.Contains (SS.Tag v)) s = predicateToSQL (SS.Contains (SS.Text v)) (s <> "[*]")
predicateToSQL (SS.Eq v) s = qUnQ s "=" v
predicateToSQL (SS.Gt v) s = qUnQ s ">" v
predicateToSQL (SS.Gte v) s = qUnQ s ">=" v
predicateToSQL (SS.Lt v) s = qUnQ s "<" v
predicateToSQL (SS.Lte v) s = qUnQ s "<=" v
predicateToSQL (SS.Ne v) s = qUnQ s "<>" v
predicateToSQL (SS.Like v) s = s <> " ~* '" <> globToRegex v <> "'"

escapeRegex :: String -> String
escapeRegex = RX.replace regexEscapeRegex "\\$&"
  where
    regexEscapeRegex =
      RX.regex
        "[\\-\\[\\]\\/\\{\\}\\(\\)\\*\\+\\?\\.\\\\\\^\\$\\|]"
        RX.noFlags { global = true }


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

range
  :: String
  -> String
  -> String
  -> String
range v v' s =
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
    SS.Range v v' -> ""

labelsProjection
  :: Array String
  -> Array SS.Label
  -> Array String
labelsProjection fields [] = RX.replace firstDot "" <$> fields
labelsProjection _ ls = RX.replace firstDot "" <$> foldl (lift2 (<>)) [""] (labelProjection <$> ls)

labelProjection :: SS.Label -> Array String
labelProjection l =
  case l of
    SS.Common "*" -> ["{*}", "[*]"]
    SS.Common "{*}" -> ["{*}"]
    SS.Common "[*]" -> ["[*]"]
    SS.Common l -> [".\"" <> l <> "\""]
    SS.Meta l -> labelProjection $ SS.Common l

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

