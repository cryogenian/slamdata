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

module SlamData.SqlSquared.Tagged where

import SlamData.Prelude

import Data.Array as A
import Data.Int as Int
import Data.Formatter.DateTime as Fd
import Data.DateTime as DT
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX

import SqlSquared (Sql)
import SqlSquared as Sql

import Utils (hush, stringToNumber, stringToBoolean)

newtype ParseError = ParseError String

derive newtype instance eqParseError ∷ Eq ParseError
derive newtype instance ordParseError ∷ Ord ParseError
derive instance newtypeParseError ∷ Newtype ParseError _

instance showParseError ∷ Show ParseError where
  show (ParseError msg) = "(ParseError " <> show msg <> ")"

dateFormat :: String
dateFormat = "YYYY-MM-DD"

timeFormat :: String
timeFormat = "HH:mm:ss"

dateTimeFormat :: String
dateTimeFormat = "YYYY-MM-DDTHH:mm:ssZ"

parseTime ∷ String → ParseError ⊹ DT.Time
parseTime = bimap ParseError DT.time ∘ Fd.unformatDateTime timeFormat

parseDate ∷ String → ParseError ⊹ DT.Date
parseDate = bimap ParseError DT.date ∘ Fd.unformatDateTime dateFormat

parseDateTime ∷ String → ParseError ⊹ DT.DateTime
parseDateTime = lmap ParseError ∘ Fd.unformatDateTime dateTimeFormat

dateSql ∷ String → Either ParseError Sql
dateSql s = do
  _ ← parseDate s
  pure $ Sql.invokeFunction "DATE" $ pure $ Sql.string s

timeSql ∷ String → Either ParseError Sql
timeSql s = do
  _ ← parseTime s
  pure $ Sql.invokeFunction "TIME" $ pure $ Sql.string s

datetimeSql ∷ String → Either ParseError Sql
datetimeSql s = do
  _ ← parseDateTime s
  pure $ Sql.invokeFunction "TIMESTAMP" $ pure $ Sql.string s

intervalSql ∷ String → Either ParseError Sql
intervalSql s = do
  if RX.test rgx s
    then pure $ Sql.invokeFunction "INTERVAL" $ pure $ Sql.string s
    else Left (ParseError ("Failed to parse interval from " <> show s))
  where
  rgx =
    URX.unsafeRegex
      "P((([0-9]*\\.?[0-9]*)Y)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)W)?(([0-9]*\\.?[0-9]*)D)?)?(T(([0-9]*\\.?[0-9]*)H)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)S)?)?"
      RXF.noFlags

oidSql ∷ String → Either ParseError Sql
oidSql =
  pure ∘ Sql.invokeFunction "OID" ∘ pure ∘ Sql.string

numSql ∷ String → Either ParseError Sql
numSql s =
 case stringToNumber s of
  Nothing → Left (ParseError ("Failed to parse number from " <> show s))
  Just num → pure $ Sql.num num

intSql ∷ String → Either ParseError Sql
intSql s =
 case Int.fromString s of
  Nothing → Left (ParseError ("Failed to parse integer from " <> show s))
  Just int → pure $ Sql.int int

boolSql ∷ String → Either ParseError Sql
boolSql s =
  case stringToBoolean s of
    Nothing → Left (ParseError ("Failed to parse boolean from " <> show s))
    Just bool → pure $ Sql.bool bool

allSqls ∷ String → Array Sql
allSqls s =
  A.mapMaybe (\f → hush (f s))
    [ dateSql, timeSql, datetimeSql, intervalSql, numSql, intSql, boolSql ]
