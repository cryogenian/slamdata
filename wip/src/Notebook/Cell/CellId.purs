module Notebook.Cell.CellId
       ( CellId(..)
       , string2cellId
       , runCellId
       ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Utils (s2i)

import Data.Generic (Generic, gEq, gCompare)

-- | The slot address value for cells and identifier within the notebook graph.
newtype CellId = CellId Int

runCellId :: CellId -> Int
runCellId (CellId i) = i

derive instance genericCellId :: Generic CellId
instance eqCellId :: Eq CellId where eq = gEq
instance ordCellId :: Ord CellId where compare = gCompare

string2cellId :: String -> Either String CellId
string2cellId str = maybe (Left "incorrect cell id") (Right <<< CellId) $ s2i str
