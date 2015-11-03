module Notebook.CellSlot
  ( CellSlot(..)
  , module Notebook.Cell.CellId
  ) where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

import Notebook.Cell.CellId (CellId())

newtype CellSlot = CellSlot CellId

derive instance genericCellSlot :: Generic CellSlot
instance eqCellSlot :: Eq CellSlot where eq = gEq
instance ordCellSlot :: Ord CellSlot where compare = gCompare
