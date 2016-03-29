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

module Test.SlamData.Property.Notebook.Cell.Markdown.Model where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (mconcat)

import SlamData.Notebook.Cell.Markdown.Model as M

import Test.StrongCheck (QC, Result(..), quickCheck, (<?>))
import Text.Markdown.SlamDown.Halogen.Component.State (SlamDownState(..))

check :: QC Unit
check = quickCheck \(SlamDownState { document, formState }) ->
  let model = { input: document, state: formState }
  in case M.decode (M.encode model) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right model' ->
      mconcat
       [ model.input == model'.input <?> "input mismatch: " <> show model.input <> " vs. " <> show model'.input
       , model.state == model'.state <?> "state mismatch: " <> show model.state <> " vs. " <> show model'.state
       ]
