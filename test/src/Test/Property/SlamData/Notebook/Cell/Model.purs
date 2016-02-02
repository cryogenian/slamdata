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

module Test.Property.SlamData.Notebook.Cell.Model
  ( ArbCell()
  , runArbCell
  , check
  , checkCellEquality
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (mconcat)

import SlamData.Notebook.Cell.Model (Model(), encode, decode)

import Test.StrongCheck (QC(), Result(..), Arbitrary, arbitrary, quickCheck, (<?>))

import Test.Property.ArbJson (runArbJson)
import Test.Property.SlamData.Notebook.Cell.CellId (runArbCellId)
import Test.Property.SlamData.Notebook.Cell.CellType (runArbCellType)

newtype ArbCell = ArbCell Model

runArbCell :: ArbCell -> Model
runArbCell (ArbCell m) = m

instance arbitraryArbCell :: Arbitrary ArbCell where
  arbitrary = do
    cellId <- runArbCellId <$> arbitrary
    cellType <- runArbCellType <$> arbitrary
    state <- runArbJson <$> arbitrary
    hasRun <- arbitrary
    pure $ ArbCell { cellId, cellType, state, hasRun }

check :: QC Unit
check = quickCheck $ runArbCell >>> \model ->
  case decode (encode model) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right model' -> checkCellEquality model model'

checkCellEquality :: Model -> Model -> Result
checkCellEquality model model' =
  mconcat
   [ model.cellId == model'.cellId <?> "cellId mismatch"
   , model.cellType == model'.cellType <?> "cellType mismatch"
   , model.state == model'.state <?> "state mismatch"
   , model.hasRun == model'.hasRun <?> "hasRun mismatch"
   ]
