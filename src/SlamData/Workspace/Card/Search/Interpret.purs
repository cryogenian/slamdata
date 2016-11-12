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

module SlamData.Workspace.Card.Search.Interpret
  ( queryToSQL
  ) where

import SlamData.Prelude

import Data.Array (filter, catMaybes, head, nub, fromFoldable)
import Data.Int as Int
import Data.List (List)
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF

import Data.Json.Extended as EJSON

import Text.SlamSearch.Types as SS

import Utils as Utils

-- TODO: We need to really obliterate this module and replace these regular
-- expressions and ad hoc renderers with something that targets a SQL^2 A(S/B)T,
-- which is then serialized separately. Blocked by SD-1391.

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
        S.joinWith ", "
      $ nub
      $ map (RX.replace firstDot "")
      $ catMaybes
      $ map join
      $ map head
      $ catMaybes
      $ map (RX.match topFieldRegex)
      $ fields

    topFieldRegex :: RX.Regex
    topFieldRegex =
      unsafePartial fromRight $
        RX.regex "^\\.[^\\.\\[]+|^\\[.+\\]/" RXF.noFlags

    whereOrFalse :: String
    whereOrFalse = if whereClause == "()" then "FALSE" else whereClause

    whereClause :: String
    whereClause =
        S.joinWith " OR "
      $ map oneWhereInput
      $ fromFoldable
      $ unwrap
      $ map (termToSQL fields) query

    oneWhereInput :: List String -> String
    oneWhereInput s = pars $ S.joinWith " AND " $ fromFoldable s

needDistinct :: String -> Boolean
needDistinct input =
  (isJust $ S.indexOf (S.Pattern "{*}") input) || (isJust $ S.indexOf (S.Pattern "[*]") input)

termToSQL
  :: Array String
  -> SS.Term
  -> String
termToSQL fields (SS.Term {include: include, predicate: p, labels: ls}) =
  if not include
    then "NOT " <> pars (termToSQL fields $ SS.Term {include: true, predicate: p, labels: ls})
    else renderPredicate $ labelsProjection fields (fromFoldable ls)
  where
    renderPredicate :: Array String -> String
    renderPredicate prj =
      S.joinWith " OR " (predicateToSQL p <$> prj)

predicateToSQL
  :: SS.Predicate
  -> String
  -> String
predicateToSQL (SS.Contains (SS.Text v)) s =
  S.joinWith " OR " $
    [s <> " ~* " <> EJSON.renderEJson (EJSON.string (globToRegex (containsToGlob v)))]
    <> (if needUnq v then renderLowercased v else [])
    <> (if not (needDateTime v) && needDate v then render date else [ ])
    <> (if needTime v then render time  else [])
    <> (if needDateTime v then render ts else [])
    <> (if needInterval v then render i else [])

  where
    containsToGlob :: String -> String
    containsToGlob v' =
      if hasSpecialChars v'
      then v'
      else "*" <> v' <> "*"

    hasSpecialChars :: String -> Boolean
    hasSpecialChars v' =
      isJust (S.indexOf (S.Pattern "*") v') || isJust (S.indexOf (S.Pattern "?") v')

    date = EJSON.renderEJson $ EJSON.date v
    time = EJSON.renderEJson $ EJSON.time v
    ts = EJSON.renderEJson $ EJSON.timestamp v
    i = EJSON.renderEJson $ EJSON.interval v

    renderLowercased v' = [ "LOWER(" <> s <> ") = " <> v']
    render v' = [s <> " = " <> v']

predicateToSQL (SS.Range (SS.Text v) (SS.Text v')) s =
  S.joinWith " OR " $
    [ forR' (quote v) (quote v') ]
    <> (if needUnq v && needUnq v' then [ forR v v' ] else [ ])
    <> (if needDate v && needDate v' then [ forR date date' ] else [ ])

  where
    date = EJSON.renderEJson $ EJSON.date v
    date' = EJSON.renderEJson $ EJSON.date v'

    forR' :: String -> String -> String
    forR' a b =
      fold ["(LOWER(", s, ") >=", a, " AND LOWER(", s, ") <= ", b, ")"]

    forR :: String -> String -> String
    forR a b =
      fold ["(", s, " >= ", a, " AND ", s, " <= ", b, ")"]

predicateToSQL (SS.Range (SS.Tag val) val') s =
  predicateToSQL (SS.Range (SS.Text val) val') s
predicateToSQL (SS.Range val (SS.Tag val')) s =
  predicateToSQL (SS.Range val (SS.Text val')) s
predicateToSQL (SS.Contains (SS.Tag v)) s =
  predicateToSQL (SS.Contains (SS.Text v)) s
predicateToSQL (SS.Eq v) s = renderBinRel s "=" v
predicateToSQL (SS.Gt v) s = renderBinRel s ">" v
predicateToSQL (SS.Gte v) s = renderBinRel s ">=" v
predicateToSQL (SS.Lt v) s = renderBinRel s "<" v
predicateToSQL (SS.Lte v) s = renderBinRel s "<=" v
predicateToSQL (SS.Ne v) s = renderBinRel s "<>" v
predicateToSQL (SS.Like v) s = s <> " ~* " <> EJSON.renderEJson (EJSON.string v)

globToRegex :: String -> String
globToRegex =
  (\x -> "^" <> x <> "$")
    <<< RX.replace askRegex "."
    <<< RX.replace starRegex ".*"
    <<< RX.replace globEscapeRegex "\\$&"
  where
    globEscapeRegex =
      unsafePartial fromRight $
        RX.regex
          "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|]"
          RXF.global

    starRegex =
      unsafePartial fromRight $
        RX.regex "\\*" RXF.global
    askRegex =
      unsafePartial fromRight $
        RX.regex "\\?" RXF.global

renderBinRel
  :: String
  -> String
  -> SS.Value
  -> String
renderBinRel s op v = pars $
  S.joinWith " OR " $
    [ forV' (quote unquoted) ]
    <> (if needUnq unquoted then [ forV unquoted ] else [])
    <> (if not (needDateTime unquoted) && needDate unquoted then [ forV date ] else [])
    <> (if needTime unquoted then [ forV time ] else [])
    <> (if needDateTime unquoted then [ forV ts ] else [])
    <> (if needInterval unquoted then [ forV i ] else [])
  where
    unquoted = valueToSQL v
    date = EJSON.renderEJson $ EJSON.date unquoted
    time = EJSON.renderEJson $ EJSON.time unquoted
    ts = EJSON.renderEJson $ EJSON.timestamp unquoted
    i = EJSON.renderEJson $ EJSON.interval unquoted

    forV' v' = fold ["LOWER(", s, ") ", op, " ", v']
    forV v' = fold [s, " ", op, " ", v']

-- | Whether the string should be rendered without quotes
needUnq :: String -> Boolean
needUnq s =
  fromMaybe false ((show >>> (_ == s)) <$> Int.fromString s)
  || fromMaybe false ((show >>> (_ == s)) <$> Utils.stringToNumber s)
  || s == "true"
  || s == "false"


needDate :: String -> Boolean
needDate = RX.test dateRegex
  where
    dateRegex =
      unsafePartial fromRight $
        RX.regex
          """^(((19|20)([2468][048]|[13579][26]|0[48])|2000)[-]02[-]29|((19|20)[0-9]{2}[-](0[4678]|1[02])[-](0[1-9]|[12][0-9]|30)|(19|20)[0-9]{2}[-](0[1359]|11)[-](0[1-9]|[12][0-9]|3[01])|(19|20)[0-9]{2}[-]02[-](0[1-9]|1[0-9]|2[0-8])))$"""
          RXF.noFlags


needTime :: String -> Boolean
needTime = RX.test timeRegex
  where
    timeRegex =
      unsafePartial fromRight $
        RX.regex
          "^([0-1]?[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$"
          RXF.noFlags


needDateTime :: String -> Boolean
needDateTime = RX.test dtRegex
  where
    dtRegex =
      unsafePartial fromRight $
        RX.regex
          "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9]) (2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"
          RXF.noFlags

needInterval :: String -> Boolean
needInterval = RX.test intervalRegex
  where
    intervalRegex =
      unsafePartial fromRight $
        RX.regex
          "P((([0-9]*\\.?[0-9]*)Y)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)W)?(([0-9]*\\.?[0-9]*)D)?)?(T(([0-9]*\\.?[0-9]*)H)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)S)?)?"
          RXF.noFlags

valueToSQL :: SS.Value -> String
valueToSQL =
  case _ of
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
    arrFieldRgx =
      unsafePartial fromRight $
        RX.regex "\\[\\d+\\]" RXF.global

labelsRegex :: Array SS.Label -> RX.Regex
labelsRegex [] = unsafePartial fromRight $ RX.regex ".*" RXF.noFlags
labelsRegex ls =
  unsafePartial fromRight $ RX.regex ("^" <> (foldMap mapFn ls) <> "$") RXF.ignoreCase
  where
  mapFn :: SS.Label -> String
  mapFn (SS.Meta l) = mapFn (SS.Common l)
  mapFn (SS.Common "{*}") = "(\\.[^\\.]+)"
  mapFn (SS.Common "[*]") = "(\\[\\d+\\])"
  mapFn (SS.Common "*") = "(\\.[^\\.]+|\\[\\d+\\])"
  mapFn (SS.Common l)
    | RX.test (unsafePartial fromRight $ RX.regex "\\[\\d+\\]" RXF.noFlags) l =
        RX.replace openSquare "\\["
      $ RX.replace closeSquare "\\]"
      $ l
    | otherwise = "(\\.`" <> l <> "`|\\." <> l <> ")"

  openSquare :: RX.Regex
  openSquare = unsafePartial fromRight $ RX.regex "\\[" RXF.noFlags

  closeSquare :: RX.Regex
  closeSquare = unsafePartial fromRight $ RX.regex "\\]" RXF.noFlags

firstDot :: RX.Regex
firstDot = unsafePartial fromRight $ RX.regex "^\\." RXF.noFlags

quote :: String -> String
quote s = EJSON.renderEJson $ EJSON.string s

pars :: String -> String
pars s = "(" <> s <> ")"
