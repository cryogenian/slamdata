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

module SlamData.Workspace.Card.Markdown.Interpret
  ( formFieldValueToVarMapValue
  , formFieldEmptyValue
  ) where

import SlamData.Prelude

import Control.Monad.Eff (runPure)
import Control.Monad.Eff.Exception (try)

import Data.Array as A
import Data.Functor.Compose (decompose)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.JSDate as JSD
import Data.Json.Extended as EJSON
import Data.List as L
import Data.Maybe as M

import SlamData.Workspace.Card.Port.VarMap as VM

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
          SD.Time _ _ → EJSON.null
          SD.DateTime _ _ → EJSON.null
      SD.CheckBoxes _ _ → EJSON.array []
      SD.RadioButtons _ _ → EJSON.null
      SD.DropDown _ _ → EJSON.null

formFieldValueToVarMapValue
  ∷ ∀ m
  . Monad m
  ⇒ SDS.FormFieldValue VM.VarMapValue
  → m (M.Maybe VM.VarMapValue)
formFieldValueToVarMapValue v =
  runMaybeT ∘ map VM.Literal $
    case v of
      SD.TextBox tb → do
        tb' ← liftMaybe $ SD.traverseTextBox decompose tb
        case tb' of
          SD.PlainText (Identity x) → pure $ EJSON.string x
          SD.Numeric (Identity x) → pure $ EJSON.decimal x
          SD.Date _ → pure ∘ EJSON.date $ SDPR.prettyPrintTextBoxValue tb'
          SD.Time _ _ → pure ∘ EJSON.time $ SDPR.prettyPrintTextBoxValue tb'
          SD.DateTime _ (Identity localDateTime) →
            pure $ EJSON.timestamp $ either (const "") id $ mkdate localDateTime
      SD.CheckBoxes (Identity sel) _ →
        pure ∘ EJSON.array ∘ A.fromFoldable $ L.mapMaybe getLiteral sel
      SD.RadioButtons (Identity x) _ →
        getLiteral x
      SD.DropDown mx _ → do
        Identity x ← liftMaybe mx
        getLiteral x

  where
    liftMaybe
      ∷ ∀ n a
      . (Applicative n)
      ⇒ Maybe a
      → MaybeT n a
    liftMaybe =
      MaybeT ∘ pure

    mkdate localDateTime = runPure $ try $
      JSD.toISOString $
        JSD.jsdate
          { year: Int.toNumber localDateTime.date.year
          , month: (Int.toNumber localDateTime.date.month) - one
          , day: Int.toNumber localDateTime.date.day
          , hour: Int.toNumber localDateTime.time.hours
          , minute: Int.toNumber localDateTime.time.minutes
          , second: Int.toNumber $ fromMaybe 0 localDateTime.time.seconds
          , millisecond: 0.0
          }
