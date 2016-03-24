module SlamData.Notebook.Cell.Next.Component.State where

import Prelude

import Data.Maybe as M
import Data.Lens (LensP(), lens)

import SlamData.Notebook.Cell.CellType as Ct

type State =
  {
    types :: Array Ct.CellType
    -- THis would be unnecessary after error card and autorun for every cell
    -- be implemented
  , message :: M.Maybe String
  }


_types :: ∀ a r. LensP {types :: a |r} a
_types = lens _.types (_{types = _})

_message :: ∀ a r. LensP {message :: a|r} a
_message = lens _.message (_{message = _})

initialState :: State
initialState =
  {
    types: Ct.nextCellTypes M.Nothing
  , message: M.Nothing
  }
