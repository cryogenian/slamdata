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

module SlamData.Notebook.Card.Markdown.Interpret
  ( formFieldValueToVarMapValue
  , formFieldEmptyValue
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Maybe.Trans as MT

import Data.Enum as Enum
import Data.Date as D
import Data.Date.Locale as DL
import Data.Time as DT
import Data.Functor.Compose (decompose)
import Data.Identity (Identity(..))
import Data.List as L
import Data.Maybe as M
import Data.Json.Extended as EJSON

import SlamData.Notebook.Card.Port.VarMap as VM

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Pretty as SDPR
import Text.Markdown.SlamDown.Halogen.Component.State as SDS

-- The use of this function in formFieldValueToVarMapValue is suspicious, and
-- lead me to think that we have not arranged our data structures properly. In
-- particular, we need to decide what it means to have a collection of SQL^2
-- query expressions. Currently, we end up just filtering them out.

-- One option that we might want to consider, in case we do intend to support things
-- like arrays of query expressions, etc. would be to have something like
-- `type VarMapValue = Mu (Coproduct EJsonF ExprF)`.

getLiteral
  ∷ ∀ m
  . (Plus m, Applicative m)
  ⇒ VM.VarMapValue
  → m EJSON.EJson
getLiteral (VM.Literal l) = pure l
getLiteral _ = empty

formFieldEmptyValue
  ∷ ∀ f a
  . SD.FormFieldP f a
  → VM.VarMapValue
formFieldEmptyValue field =
  VM.Literal
    case field of
      SD.TextBox tb →
        case tb of
          SD.PlainText _ → EJSON.string ""
          SD.Numeric _ → EJSON.integer 0
          SD.Date _ → EJSON.null
          SD.Time _ → EJSON.null
          SD.DateTime _ → EJSON.null
      SD.CheckBoxes _ _ → EJSON.array []
      SD.RadioButtons _ _ → EJSON.array []
      SD.DropDown _ _ → EJSON.array []

formFieldValueToVarMapValue
  ∷ ∀ e m
  . (MonadEff (locale ∷ DL.Locale | e) m, Applicative m)
  ⇒ SDS.FormFieldValue VM.VarMapValue
  → m (M.Maybe VM.VarMapValue)
formFieldValueToVarMapValue v =
  MT.runMaybeT ∘ map VM.Literal $
    case v of
      SD.TextBox tb → do
        tb' ← liftMaybe $ SD.traverseTextBox decompose tb
        case tb' of
          SD.PlainText (Identity x) → pure $ EJSON.string x
          SD.Numeric (Identity x) → pure $ EJSON.decimal x
          SD.Date _ → pure ∘ EJSON.date $ SDPR.prettyPrintTextBoxValue tb'
          SD.Time _ → pure ∘ EJSON.time $ SDPR.prettyPrintTextBoxValue tb'
          SD.DateTime (Identity localDateTime) → do
            let
              year = D.Year localDateTime.date.year
              day = D.DayOfMonth localDateTime.date.day
              hour = DT.HourOfDay localDateTime.time.hours
              minute = DT.MinuteOfHour localDateTime.time.minutes
              second = DT.SecondOfMinute 0
              millisecond = DT.MillisecondOfSecond 0
            month ← liftMaybe $ Enum.toEnum localDateTime.date.month
            dateTime ← MT.MaybeT ∘ liftEff $ DL.dateTime year month day hour minute second millisecond
            pure ∘ EJSON.timestamp $ D.toISOString dateTime
      SD.CheckBoxes (Identity bs) (Identity xs) →
        L.zip bs xs
          # L.filter fst
          # L.mapMaybe (getLiteral ∘ snd)
          # L.fromList
          # EJSON.array
          # pure
      SD.RadioButtons (Identity x) _ →
        pure ∘ EJSON.array $ getLiteral x
      SD.DropDown mx _ → do
        Identity x ← liftMaybe mx
        pure ∘ EJSON.array $ getLiteral x

  where
    liftMaybe
      ∷ ∀ n a
      . (Applicative n)
      ⇒ Maybe a
      → MT.MaybeT n a
    liftMaybe =
      MT.MaybeT ∘ pure
