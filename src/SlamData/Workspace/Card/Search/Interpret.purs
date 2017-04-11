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
  ( queryToSql
  ) where

import SlamData.Prelude

import Data.Foldable as F
import Data.Int as Int
import Data.Json.Extended as Ej
import Data.Lens ((.~), (?~))
import Data.List ((:))
import Data.List as L
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX

import Text.SlamSearch.Types as SS

import Matryoshka (Algebra, embed, cata, Transform, transAna, ana, Coalgebra)

import Quasar.Types (FilePath)

import SqlSquare (Sql, SqlF(..))
import SqlSquare as Sql

import Utils as Utils


queryToSql
  ∷ L.List Sql
  → SS.SearchQuery
  → FilePath
  → Sql
queryToSql fields searchQuery path =
  Sql.buildSelect
    $ (Sql._isDistinct .~ isDistinct searchQuery)
    ∘ (Sql._projections .~ projections fields)
    ∘ (Sql._relations ?~ Sql.TableRelation { alias: Nothing, path: Left path })
    ∘ (Sql._filter ?~ filter fields searchQuery)

isDistinct ∷ SS.SearchQuery → Boolean
isDistinct = F.any isDistinctTerm
  where
  isDistinctTerm ∷ SS.Term → Boolean
  isDistinctTerm (SS.Term { labels }) = F.any isDistinctLabel labels

  isDistinctLabel ∷ SS.Label → Boolean
  isDistinctLabel l = case ssLabelString l of
    "[*]" → true
    "{*}" → true
    "*" → true
    s → isJust $ labelInt s

labelInt ∷ String → Maybe Int
labelInt s =
  Int.fromString s
  <|> (S.stripSuffix (S.Pattern "]") s
       >>= S.stripPrefix (S.Pattern "[")
       >>= Int.fromString)


ssLabelString ∷ SS.Label → String
ssLabelString = case _ of
  SS.Meta s → s
  SS.Common s → s

ssValueString ∷ SS.Value → String
ssValueString = case _ of
  SS.Text v → v
  SS.Tag v → v


topFieldF ∷ Algebra (SqlF Ej.EJsonF) (Maybe Sql)
topFieldF = case _ of
  Splice Nothing → Just $ embed $ Splice Nothing
  Ident s → Just $ embed $ Ident s
  Literal (Ej.Integer i) → Just $ embed $ Literal $ Ej.Integer i
  Literal (Ej.String s) → Just $ embed $ Literal $ Ej.String s
  Binop { op: Sql.FieldDeref, lhs } → lhs
  Binop { op: Sql.IndexDeref, lhs } → lhs
  Unop { expr } → expr
  _ → Nothing

projections ∷ L.List Sql → L.List (Sql.Projection Sql)
projections =
  map Sql.projection --(Sql.Projection ∘ { expr: _, alias: Nothing})
  ∘ L.nub
  ∘ L.catMaybes
  ∘ map (cata topFieldF)

filter ∷ L.List Sql → SS.SearchQuery → Sql
filter fs =
  ands
  ∘ map ors
  ∘ unwrap
  ∘ map (termToSql $ map flattenIndex fs)

ands ∷ L.List Sql → Sql
ands = case _ of
  L.Nil → Sql.bool true
  hd : L.Nil → Sql.pars hd
  hd : tl → F.foldl (\acc sql → Sql.binop Sql.And acc $ Sql.pars sql) hd tl

ors ∷ L.List Sql → Sql
ors = case _ of
  L.Nil → Sql.bool false
  hd : L.Nil → Sql.pars hd
  hd : tl → F.foldl (\acc sql → Sql.binop Sql.Or acc $ Sql.pars sql) hd tl

flattenIndex ∷ Sql → Sql
flattenIndex = transAna flattenIndexT

flattenIndexT ∷ ∀ t. Transform t (SqlF Ej.EJsonF) (SqlF Ej.EJsonF)
flattenIndexT = case _ of
  Binop { op: Sql.IndexDeref, lhs } → Unop { op: Sql.FlattenArrayValues, expr: lhs }
  s → s

termToSql ∷ L.List Sql → SS.Term → Sql
termToSql fs (SS.Term { include, predicate, labels })
  | not include =
      Sql.unop Sql.Not $ termToSql fs $ SS.Term { include: true, predicate, labels }
  | otherwise =
      ors
        $ map (predicateToSql predicate)
        $ if L.null labels then fs else pure $ labelsToField labels

labelsToField ∷ L.List SS.Label → Sql
labelsToField = ana labelToFieldF ∘ map ssLabelString ∘ L.reverse

labelToFieldF ∷ Coalgebra (SqlF Ej.EJsonF) (L.List String)
labelToFieldF = case _ of
  L.Nil → Sql.Splice Nothing
  hd : L.Nil → case labelInt hd of
    Just i → Sql.Literal $ Ej.Integer i
    Nothing → Sql.Ident hd
  hd : tl → case labelInt hd of
    Just i → Sql.Binop { op: Sql.IndexDeref,  lhs: tl, rhs: pure hd }
    Nothing → case hd of
      "[*]" → Sql.Unop { op: Sql.FlattenArrayValues, expr: tl }
      "{*}" → Sql.Unop { op: Sql.FlattenMapValues, expr: tl }
      "*" → Sql.Unop { op: Sql.FlattenMapValues, expr: tl }
      a → Sql.Binop { op: Sql.FieldDeref, lhs: tl, rhs: pure hd }


predicateToSql ∷ SS.Predicate → Sql → Sql
predicateToSql pr field = case pr of
  SS.Contains (SS.Tag v) →
    predicateToSql (SS.Contains $ SS.Text v) field
  SS.Contains (SS.Text v) →
    ors
    $ (pure
         $ Sql.invokeFunction "SEARCH"
         $ field
         : (Sql.string $ globToRegex $ containsToGlob v)
         : Sql.bool true
         : L.Nil )
    ⊕ (sqlFromSearchStr v <#> Sql.binop Sql.Eq field)
  SS.Range (SS.Tag val) vv →
    predicateToSql (SS.Range (SS.Text val) vv) field
  SS.Range vv (SS.Tag val) →
    predicateToSql (SS.Range vv $ SS.Text val) field
  SS.Range (SS.Text v) (SS.Text vv) →
    ors
    $ ( pure $ Sql.binop Sql.And
        ( Sql.pars $ Sql.binop Sql.Ge (lower field) (lower $ Sql.string v))
        ( Sql.pars $ Sql.binop Sql.Le (lower field) (lower $ Sql.string vv)))
    ⊕ do
      start ← sqlFromSearchStr v
      end ← sqlFromSearchStr vv
      pure $ Sql.binop Sql.And
        ( Sql.pars $ Sql.binop Sql.Ge field start )
        ( Sql.pars $ Sql.binop Sql.Le field end )
  SS.Eq v → renderBinRel field Sql.Eq $ ssValueString v
  SS.Gt v → renderBinRel field Sql.Gt $ ssValueString v
  SS.Lt v → renderBinRel field Sql.Lt $ ssValueString v
  SS.Ne v → renderBinRel field Sql.Neq $ ssValueString v
  SS.Gte v → renderBinRel field Sql.Ge $ ssValueString v
  SS.Lte v → renderBinRel field Sql.Le $ ssValueString v
  SS.Like v →
    Sql.invokeFunction "SEARCH"
      $ field : Sql.string v : Sql.bool true : L.Nil

renderBinRel ∷ Sql → Sql.BinaryOperator → String → Sql
renderBinRel field op v =
  ors
  $ ( pure $ Sql.binop op (lower field) (lower $ Sql.string v))
  ⊕ ( sqlFromSearchStr v <#> Sql.binop op field )

sqlFromSearchStr ∷ String → L.List Sql
sqlFromSearchStr v =
  (flip F.foldMap (Utils.stringToNumber v) $ pure ∘ Sql.num)
  ⊕ (flip F.foldMap (Int.fromString v) $ pure ∘ Sql.int)
  ⊕ (flip F.foldMap (Utils.stringToBoolean v) $ pure ∘ Sql.bool)
  ⊕ ((guard ((not $ needDateTime v) && needDate v)) $>
       Sql.invokeFunction "DATE" (Sql.string v : L.Nil))
  ⊕ (guard (needTime v) $>
       Sql.invokeFunction "TIME" (Sql.string v : L.Nil))
  ⊕ (guard (needDateTime v) $>
       Sql.invokeFunction "TIMESTAMP" (Sql.string v : L.Nil))
  ⊕ (guard (needInterval v) $>
       Sql.invokeFunction "INTERVAL" (Sql.string v : L.Nil))

lower ∷ Sql → Sql
lower = Sql.invokeFunction "LOWER" ∘ pure

globToRegex ∷ String → String
globToRegex =
  (\x → "^" <> x <> "$")
    ∘ RX.replace askRegex "."
    ∘ RX.replace starRegex ".*"
    ∘ RX.replace globEscapeRegex "\\$&"
  where
  globEscapeRegex =
    URX.unsafeRegex
    "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|]"
    RXF.global

  starRegex =
    URX.unsafeRegex
    "\\*" RXF.global
  askRegex =
    URX.unsafeRegex
    "\\?" RXF.global

containsToGlob ∷ String → String
containsToGlob v
  | hasSpecialChars v = v
  | otherwise = "*" <> v <> "*"

hasSpecialChars ∷ String → Boolean
hasSpecialChars v =
  isJust (S.indexOf (S.Pattern "*") v) || isJust (S.indexOf (S.Pattern "?") v)


needDate ∷ String → Boolean
needDate = RX.test dateRegex
  where
  dateRegex =
    URX.unsafeRegex
      """^(((19|20)([2468][048]|[13579][26]|0[48])|2000)[-]02[-]29|((19|20)[0-9]{2}[-](0[4678]|1[02])[-](0[1-9]|[12][0-9]|30)|(19|20)[0-9]{2}[-](0[1359]|11)[-](0[1-9]|[12][0-9]|3[01])|(19|20)[0-9]{2}[-]02[-](0[1-9]|1[0-9]|2[0-8])))$"""
      RXF.noFlags


needTime ∷ String → Boolean
needTime = RX.test timeRegex
  where
  timeRegex =
    URX.unsafeRegex
      "^([0-1]?[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$"
      RXF.noFlags


needDateTime ∷ String → Boolean
needDateTime = RX.test dtRegex
  where
  dtRegex =
    URX.unsafeRegex
      "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9]) (2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"
      RXF.noFlags

needInterval ∷ String → Boolean
needInterval = RX.test intervalRegex
  where
  intervalRegex =
    URX.unsafeRegex
      "P((([0-9]*\\.?[0-9]*)Y)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)W)?(([0-9]*\\.?[0-9]*)D)?)?(T(([0-9]*\\.?[0-9]*)H)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)S)?)?"
      RXF.noFlags
