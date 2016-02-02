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

module Test.Property.SlamData.Notebook.Model
  ( ArbNotebook()
  , runArbNotebook
  , check
  ) where

import Prelude

import Data.Array (zipWith)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (mconcat)
import Data.Map as M

import SlamData.Notebook.Editor.Model as Model

import Test.StrongCheck (QC(), Result(..), Arbitrary, arbitrary, quickCheck, (<?>))

import Test.Property.SlamData.Notebook.Cell.Model (runArbCell, checkCellEquality)
import Test.Property.SlamData.Notebook.Cell.CellId (runArbCellId)

newtype ArbNotebook = ArbNotebook Model.Notebook

runArbNotebook :: ArbNotebook -> Model.Notebook
runArbNotebook (ArbNotebook m) = m

instance arbitraryArbNotebook :: Arbitrary ArbNotebook where
  arbitrary = do
    cells <- map runArbCell <$> arbitrary
    dependencies <- M.fromList <<< map (bimap runArbCellId runArbCellId) <$> arbitrary
    pure $ ArbNotebook { cells, dependencies }

check :: QC Unit
check = quickCheck $ runArbNotebook >>> \model ->
  case Model.decode (Model.encode model) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right model' ->
      mconcat
       [ model.dependencies == model'.dependencies <?> "dependencies mismatch"
       , mconcat (zipWith checkCellEquality model.cells model'.cells)
       ]
