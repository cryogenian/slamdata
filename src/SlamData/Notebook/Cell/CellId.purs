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

module SlamData.Notebook.Cell.CellId
  ( CellId(..)
  , stringToCellId
  , cellIdToString
  , runCellId
  ) where

import Prelude

import Data.Argonaut (DecodeJson, EncodeJson, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic (Generic, gEq, gCompare)
import Data.Int as Int
import Data.Maybe (maybe)

-- | The slot address value for cells and identifier within the notebook graph.
newtype CellId = CellId Int

runCellId :: CellId -> Int
runCellId (CellId i) = i

stringToCellId :: String -> Either String CellId
stringToCellId = maybe (Left "incorrect cell id") (Right <<< CellId) <<< Int.fromString

cellIdToString :: CellId -> String
cellIdToString = show <<< runCellId

derive instance genericCellId :: Generic CellId
instance eqCellId :: Eq CellId where eq = gEq
instance ordCellId :: Ord CellId where compare = gCompare

instance encodeJsonCellId :: EncodeJson CellId where
  encodeJson = encodeJson <<< runCellId

instance decodeJsonCellId :: DecodeJson CellId where
  decodeJson json = CellId <$> decodeJson json

instance semiringCellId :: Semiring CellId where
  zero = CellId zero
  one = CellId one
  add (CellId a) (CellId b) = CellId $ a + b
  mul (CellId a) (CellId b) = CellId $ a * b
