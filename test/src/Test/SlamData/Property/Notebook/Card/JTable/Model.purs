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

module Test.SlamData.Property.Notebook.Card.JTable.Model
  ( ArbModel
  , runArbModel
  , ArbResult
  , runArbResult
  , check
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (mconcat)
import Data.Maybe (Maybe(..))

import SlamData.Notebook.Card.JTable.Model as M

import Test.StrongCheck (QC, Result(..), class Arbitrary, arbitrary, quickCheck, (<?>))

import Test.Property.ArbJson (runArbJson)
import Test.SlamData.Property.FileSystem.Resource (runArbResource)

newtype ArbModel = ArbModel M.Model

runArbModel :: ArbModel -> M.Model
runArbModel (ArbModel m) = m

instance arbitraryArbModel :: Arbitrary ArbModel where
  arbitrary = do
    input <- map runArbResource <$> arbitrary
    result <- map runArbResult <$> arbitrary
    pure $ ArbModel { input, result }

newtype ArbResult = ArbResult M.Result

runArbResult :: ArbResult -> M.Result
runArbResult (ArbResult m) = m

instance arbitraryArbResult :: Arbitrary ArbResult where
  arbitrary = do
    json <- runArbJson <$> arbitrary
    page <- arbitrary
    pageSize <- arbitrary
    pure $ ArbResult { json, page, pageSize }

check :: QC Unit
check = quickCheck $ runArbModel >>> \model ->
  case M.decode (M.encode model) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right model' ->
      mconcat
       [ model.input == model'.input <?> "input mismatch"
       , compareResult model.result model'.result
       ]
  where
  compareResult Nothing Nothing = Success
  compareResult (Just r) (Just r') =
    mconcat
     [ show r.json == show r'.json <?> "json mismatch"
     , r.page == r'.page <?> "page mismatch"
     , r.pageSize == r'.pageSize <?> "pageSize mismatch"
     ]
  compareResult r r' = Failed "result mismatch"
