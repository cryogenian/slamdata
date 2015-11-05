module Notebook.Cell.CellId (CellId(..), runCellId) where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

-- | The slot address value for cells and identifier within the notebook graph.
newtype CellId = CellId Int

runCellId :: CellId -> Int
runCellId (CellId i) = i

derive instance genericCellId :: Generic CellId
instance eqCellId :: Eq CellId where eq = gEq
instance ordCellId :: Ord CellId where compare = gCompare
