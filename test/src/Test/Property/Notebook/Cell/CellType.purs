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

module Test.Property.Notebook.Cell.CellType where

import Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (Either(..))
import Data.List (toList)

import Notebook.Cell.CellType (CellType(..), AceMode(..))

import Test.StrongCheck (QC(), Result(..), Arbitrary, quickCheck, (<?>))
import Test.StrongCheck.Gen (elements)

newtype ArbCellType = ArbCellType CellType

runArbCellType :: ArbCellType -> CellType
runArbCellType (ArbCellType m) = m

instance arbitraryArbCellType :: Arbitrary ArbCellType where
  arbitrary =
    ArbCellType <$>
      elements
        Explore
        (toList
          [ Ace MarkdownMode
          , Ace SQLMode
          , Search
          , Viz
          , Chart
          , Markdown
          , JTable
          ])

check :: QC Unit
check = quickCheck $ runArbCellType >>> \ct ->
  case decodeJson (encodeJson ct) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right ct' -> ct == ct' <?> "CellType failed to decode as encoded value"
