module Model.Notebook.Cell.Explore where

import Data.Int
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir)
import Model.Resource
import Optic.Core (LensP(), lens)

newtype ExploreRec =
  ExploreRec {}

initialExploreRec = ExploreRec {}
