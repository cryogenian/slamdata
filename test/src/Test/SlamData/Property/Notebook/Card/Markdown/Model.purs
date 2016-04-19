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

module Test.SlamData.Property.Notebook.Card.Markdown.Model where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe as Unsafe

import Data.Date.Locale as DL
import Data.Either (Either(..))
import Data.Foldable (mconcat)
import Data.List as L
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Notebook.Card.Markdown.Model as M
import SlamData.Notebook.Card.Markdown.Component.State as MDS

import Text.Markdown.SlamDown.Halogen.Component.State as SDS

import Test.StrongCheck (QC, Result(..), quickCheck, (<?>))

checkSerialization ∷ QC Unit
checkSerialization =
  quickCheck \(SDS.SlamDownState { document, formState }) →
    let model = { input: document, state: formState }
    in case M.decode (M.encode model) of
      Left err → Failed $ "Decode failed: " ++ err
      Right model' →
        mconcat
         [ model.input == model'.input <?> "input mismatch: " <> show model.input <> " vs. " <> show model'.input
         , model.state == model'.state <?> "state mismatch: " <> show model.state <> " vs. " <> show model'.state
         ]

unsafeRunLocale
  ∷ ∀ a
  . Eff (locale ∷ DL.Locale) a
  → a
unsafeRunLocale =
  Unsafe.unsafePerformEff

checkVarMapConstruction ∷ QC Unit
checkVarMapConstruction =
  quickCheck \(SDS.SlamDownState { document, formState }) →
    let
      formDesc = SDS.formDescFromDocument document
      varMap = unsafeRunLocale $ MDS.formStateToVarMap formDesc formState
      descKeys = Set.fromList $ L.toList $ SM.keys formDesc
      stateKeys = Set.fromList $ L.toList $ SM.keys varMap
    in
      descKeys == stateKeys
        <?> ("Keys mismatch: " <> show descKeys <> " vs. " <> show stateKeys)

check ∷ QC Unit
check = do
  checkVarMapConstruction
  checkSerialization
