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

module Test.Property.SlamData.Notebook.Cell.CellId where

import Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (Either(..))

import SlamData.Notebook.Cell.CellId (CellId(..))

import Test.StrongCheck (QC(), Result(..), Arbitrary, arbitrary, quickCheck, (<?>))

newtype ArbCellId = ArbCellId CellId

runArbCellId :: ArbCellId -> CellId
runArbCellId (ArbCellId m) = m

instance arbitraryArbCellId :: Arbitrary ArbCellId where
  arbitrary = ArbCellId <<< CellId <$> arbitrary

check :: QC Unit
check = quickCheck $ runArbCellId >>> \ci ->
  case decodeJson (encodeJson ci) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right ci' -> ci == ci' <?> "CellId failed to decode as encoded value"
