module Notebook.Cell.CellId (CellId(..)) where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

-- | The slot address value for cells and identifier within the notebook graph.
newtype CellId = CellId Int

derive instance genericCellId :: Generic CellId
instance eqCellId :: Eq CellId where eq = gEq
instance ordCellId :: Ord CellId where compare = gCompare
