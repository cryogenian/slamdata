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

module Test.SlamData.Property.Workspace.Card.Markdown.Model where

import SlamData.Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe as Unsafe

import Data.JSDate (LOCALE)
import Data.Json.Extended as EJSON
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Workspace.Card.Markdown.Model as M
import SlamData.Workspace.Card.Markdown.Component.State as MDS
import SlamData.Workspace.Card.Port.VarMap as VM

import Text.Markdown.SlamDown.Halogen.Component.State as SDS

import Test.StrongCheck (SC, Result(..), quickCheck, (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)

newtype JsonEncodableVarMapValue = JsonEncodableVarMapValue VM.VarMapValue

derive instance eqJsonEncodableVarMapValue ∷ Eq JsonEncodableVarMapValue

derive instance ordJsonEncodableVarMapValue ∷ Ord JsonEncodableVarMapValue

getJsonEncodableVarMapValue
  ∷ JsonEncodableVarMapValue
  → VM.VarMapValue
getJsonEncodableVarMapValue (JsonEncodableVarMapValue x) =
  x

instance arbitraryJsonEncodableVarMapValue ∷ Arbitrary JsonEncodableVarMapValue where
  arbitrary =
    JsonEncodableVarMapValue <$> do
      VM.Literal <$> EJSON.arbitraryJsonEncodableEJsonOfSize 1
        <|> VM.QueryExpr <$> arbitrary

checkSerialization ∷ forall eff. SC eff Unit
checkSerialization =
  quickCheck $ map getJsonEncodableVarMapValue >>> \(SDS.SlamDownState { document, formState }) →
    let model = { input: document, state: formState }
    in case M.decode (M.encode model) of
      Left err → Failed $ "Decode failed: " <> err
      Right model' →
        fold
         [ model.input == model'.input <?> "input mismatch: " <> show model.input <> " vs. " <> show model'.input
         , model.state == model'.state <?> "state mismatch: " <> show model.state <> " vs. " <> show model'.state
         ]

unsafeRunLocale
  ∷ ∀ a
  . Eff (locale ∷ LOCALE) a
  → a
unsafeRunLocale =
  Unsafe.unsafePerformEff

checkVarMapConstruction ∷ forall eff. SC eff Unit
checkVarMapConstruction =
  quickCheck \(SDS.SlamDownState { document, formState }) →
    let
      inputState = SDS.formStateFromDocument document
      varMap = unsafeRunLocale $ MDS.formStateToVarMap inputState formState
      descKeys = Set.fromFoldable $ SM.keys inputState
      stateKeys = Set.fromFoldable $ SM.keys varMap
    in
      descKeys == stateKeys
        <?> ("Keys mismatch: " <> show descKeys <> " vs. " <> show stateKeys)

check ∷ forall eff. SC eff Unit
check = do
  checkVarMapConstruction
  checkSerialization
