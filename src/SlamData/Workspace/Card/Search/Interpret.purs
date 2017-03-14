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
import Data.Json.Extended.Signature.Parse as EJP

import Text.Parsing.Parser as P
import Text.SlamSearch.Types as SS

import Utils as Utils

-- TODO: We need to really obliterate this module and replace these regular
-- expressions and ad hoc renderers with something that targets a SQL^2 A(S/B)T,
-- which is then serialized separately. Blocked by SD-1391.

queryToSQL
  ∷ Array String
  → SS.SearchQuery
  → String
queryToSQL fields query =
     "SELECT"
  <> (if needDistinct whereClause then " DISTINCT " else " ")
  <> topFields
  <> " from {{path}} where " <> whereOrFalse
  where
    topFields ∷ String
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

    topFieldRegex ∷ RX.Regex
    topFieldRegex =
      unsafePartial fromRight $
        RX.regex "^\\.[^\\.\\[]+|^\\[.+\\]/" RXF.noFlags

    whereOrFalse ∷ String
    whereOrFalse = if whereClause == "()" then "FALSE" else whereClause

    whereClause ∷ String
    whereClause =
        S.joinWith " OR "
      $ map oneWhereInput
      $ fromFoldable
      $ unwrap
      $ map (termToSQL fields) query

    oneWhereInput ∷ List String → String
    oneWhereInput s = pars $ S.joinWith " AND " $ fromFoldable s

needDistinct ∷ String → Boolean
needDistinct input =
  (isJust $ S.indexOf (S.Pattern "{*}") input) || (isJust $ S.indexOf (S.Pattern "[*]") input)

termToSQL
  ∷ Array String
  → SS.Term
  → String
termToSQL fields (SS.Term {include: include, predicate: p, labels: ls}) =
  if not include
    then "NOT " <> pars (termToSQL fields $ SS.Term {include: true, predicate: p, labels: ls})
    else renderPredicate $ labelsProjection fields (fromFoldable ls)
  where
    renderPredicate ∷ Array String → String
    renderPredicate prj =
      S.joinWith " OR " (predicateToSQL p <$> prj)

predicateToSQL
  ∷ SS.Predicate
  → String
  → String
predicateToSQL (SS.Contains (SS.Text v)) s =
  S.joinWith " OR " $
    [s <> " ~* " <> EJSON.renderEJson (EJSON.string (globToRegex (containsToGlob v)))]
    <> (if needUnq v then renderLowercased v else [])
    <> (if not isJust ts then maybe [] render date else [])
    <> (maybe [] render time)
    <> (maybe [] render ts)
    <> (if needInterval v then render i else [])

  where
    containsToGlob ∷ String → String
    containsToGlob v' =
      if hasSpecialChars v'
      then v'
      else "*" <> v' <> "*"

    hasSpecialChars ∷ String → Boolean
    hasSpecialChars v' =
      isJust (S.indexOf (S.Pattern "*") v') || isJust (S.indexOf (S.Pattern "?") v')

    date = parseEJsonValue EJSON.date EJP.parseDate v
    time = parseEJsonValue EJSON.time EJP.parseTime v
    ts = parseEJsonValue EJSON.timestamp EJP.parseTimestamp v
    i = EJSON.renderEJson $ EJSON.interval v

    renderLowercased v' = [ "LOWER(" <> s <> ") = " <> v']
    render v' = [s <> " = " <> v']

predicateToSQL (SS.Range (SS.Text v) (SS.Text v')) s =
  S.joinWith " OR " $
    [ forR' (quote v) (quote v') ]
    <> (if needUnq v && needUnq v' then [ forR v v' ] else [ ])
    <> case date, date' of
      Just d, Just d' → [ forR d d' ]
      _, _ → []

  where
    date = parseEJsonValue EJSON.date EJP.parseDate v
    date' = parseEJsonValue EJSON.date EJP.parseDate v'

    forR' ∷ String → String → String
    forR' a b =
      fold ["(LOWER(", s, ") >=", a, " AND LOWER(", s, ") <= ", b, ")"]

    forR ∷ String → String → String
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

globToRegex ∷ String → String
globToRegex =
  (\x → "^" <> x <> "$")
    ∘ RX.replace askRegex "."
    ∘ RX.replace starRegex ".*"
    ∘ RX.replace globEscapeRegex "\\$&"
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
  ∷ String
  → String
  → SS.Value
  → String
renderBinRel s op v = pars $
  S.joinWith " OR " $
    [ forV' (quote unquoted) ]
    <> (if needUnq unquoted then [ forV unquoted ] else [])
    <> (maybe [] (pure ∘ forV) time)
    <> (if not isJust ts then maybe [] (pure ∘ forV) date else [])
    <> (maybe [] (pure ∘ forV) ts)
    <> (if needInterval unquoted then [ forV i ] else [])
  where
    unquoted = valueToSQL v
    date = parseEJsonValue EJSON.date EJP.parseDate unquoted
    time = parseEJsonValue EJSON.time EJP.parseTime unquoted
    ts = parseEJsonValue EJSON.timestamp EJP.parseTimestamp unquoted
    i = EJSON.renderEJson $ EJSON.interval unquoted
    forV' v' = fold ["LOWER(", s, ") ", op, " ", v']
    forV v' = fold [s, " ", op, " ", v']

parseEJsonValue ∷ ∀ a. (a → EJSON.EJson) → P.Parser String a → String → Maybe String
parseEJsonValue f p input =
  either (const Nothing) (Just ∘ EJSON.renderEJson ∘ f) $ P.runParser input p

-- | Whether the string should be rendered without quotes
needUnq ∷ String → Boolean
needUnq s =
  fromMaybe false ((show >>> (_ == s)) <$> Int.fromString s)
  || fromMaybe false ((show >>> (_ == s)) <$> Utils.stringToNumber s)
  || s == "true"
  || s == "false"

needInterval ∷ String → Boolean
needInterval = RX.test intervalRegex
  where
    intervalRegex =
      unsafePartial fromRight $
        RX.regex
          "P((([0-9]*\\.?[0-9]*)Y)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)W)?(([0-9]*\\.?[0-9]*)D)?)?(T(([0-9]*\\.?[0-9]*)H)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)S)?)?"
          RXF.noFlags

valueToSQL ∷ SS.Value → String
valueToSQL =
  case _ of
    SS.Text v → v
    SS.Tag v → v

labelsProjection ∷ Array String → Array SS.Label → Array String
labelsProjection fields ls =
  nub
  $ RX.replace arrFieldRgx "[*]"
  <$> RX.replace firstDot ""
  <$> filter (RX.test $ labelsRegex ls) fields
  where
    arrFieldRgx ∷ RX.Regex
    arrFieldRgx =
      unsafePartial fromRight $
        RX.regex "\\[\\d+\\]" RXF.global

labelsRegex ∷ Array SS.Label → RX.Regex
labelsRegex [] = unsafePartial fromRight $ RX.regex ".*" RXF.noFlags
labelsRegex ls =
  unsafePartial fromRight $ RX.regex ("^" <> (foldMap mapFn ls) <> "$") RXF.ignoreCase
  where
  mapFn ∷ SS.Label → String
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

  openSquare ∷ RX.Regex
  openSquare = unsafePartial fromRight $ RX.regex "\\[" RXF.noFlags

  closeSquare ∷ RX.Regex
  closeSquare = unsafePartial fromRight $ RX.regex "\\]" RXF.noFlags

firstDot ∷ RX.Regex
firstDot = unsafePartial fromRight $ RX.regex "^\\." RXF.noFlags

quote ∷ String → String
quote s = EJSON.renderEJson $ EJSON.string s

pars ∷ String → String
pars s = "(" <> s <> ")"
