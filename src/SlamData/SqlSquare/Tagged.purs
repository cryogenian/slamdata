{-
Copyright 2017 SlamData, Inc.

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

module SlamData.SqlSquare.Tagged where

import SlamData.Prelude

import Data.Array as A
import Data.Int as Int
import Data.Formatter.DateTime as Fd
import Data.DateTime as DT
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX

import SqlSquare (Sql)
import SqlSquare as Sql

import Utils (stringToNumber, stringToBoolean)

parseTime ∷ String → String ⊹ DT.Time
parseTime = map DT.time ∘ Fd.unformatDateTime "HH:mm:ss"

parseDate ∷ String → String ⊹ DT.Date
parseDate = map DT.date ∘ Fd.unformatDateTime "YYYY-MM-DD"

parseDateTime ∷ String → String ⊹ DT.DateTime
parseDateTime = Fd.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ"

dateSql ∷ String → Maybe Sql
dateSql s = do
  guard $ isRight $ parseDate s
  pure $ Sql.invokeFunction "DATE" $ pure $ Sql.string s

timeSql ∷ String → Maybe Sql
timeSql s = do
  guard $ isRight $ parseTime s
  pure $ Sql.invokeFunction "TIME" $ pure $ Sql.string s

datetimeSql ∷ String → Maybe Sql
datetimeSql s = do
  guard $ isRight $ parseDateTime s
  pure $ Sql.invokeFunction "TIMESTAMP" $ pure $ Sql.string s

intervalSql ∷ String → Maybe Sql
intervalSql s = do
  guard $ RX.test rgx s
  pure $ Sql.invokeFunction "INTERVAL" $ pure $ Sql.string s
  where
  rgx =
    URX.unsafeRegex
      "P((([0-9]*\\.?[0-9]*)Y)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)W)?(([0-9]*\\.?[0-9]*)D)?)?(T(([0-9]*\\.?[0-9]*)H)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)S)?)?"
      RXF.noFlags

numSql ∷ String → Maybe Sql
numSql s = do
 num ← stringToNumber s
 pure $ Sql.num num

intSql ∷ String → Maybe Sql
intSql s = do
  int ← Int.fromString s
  pure $ Sql.int int

boolSql ∷ String → Maybe Sql
boolSql s = do
  bool ← stringToBoolean s
  pure $ Sql.bool bool

allSqls ∷ String → Array Sql
allSqls s =
  A.mapMaybe (\f → f s)
    [ dateSql, timeSql, datetimeSql, intervalSql, numSql, intSql, boolSql ]
