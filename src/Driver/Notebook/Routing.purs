module Driver.Notebook.Routing where

import Data.Either
import Data.Tuple
import Control.Apply
import Control.Alt
import Model.Notebook
import Model.Action
import Data.Foldable
import Config (notebookExtension)
import qualified Routing as R
import qualified Routing.Match as R
import qualified Routing.Match.Class as R
import qualified Routing.Hash as R
import qualified Data.String as Str
--import qualified Model.Resource as M
import Model.Resource (newNotebook, setPath, Resource())
import Data.Path.Pathy

data Routes
  = CellRoute Resource CellId Action
  | NotebookRoute Resource Action


routing :: R.Match Routes
routing = CellRoute <$> notebook <*> (R.lit "cells" *> cellId) <*> action
          <|>
          NotebookRoute <$> notebook <*> action
  where
  partsAndName = Tuple <$> (oneSlash *> (R.list notName)) <*> name

  notebookFromParts (Tuple ps name) =
    newNotebook `setPath` (Left $ 
                           (foldl (</>) rootDir (dir <$> ps)) </> file name)

  notebook :: R.Match Resource
  notebook = notebookFromParts <$> partsAndName 
        
        
  oneSlash = R.lit ""
  notebookName input =
    if Str.indexOf notebookExtension input == -1 then
      Left input
    else Right input
  pathPart input =
    if input == "" || Str.indexOf notebookExtension input /= -1 then
      Left "incorrect path part"
    else Right input

  name = R.eitherMatch (notebookName <$> R.str)
  notName = R.eitherMatch (pathPart <$> R.str)

  action = (R.eitherMatch (string2action <$> R.str)) <|> pure View
  cellId = R.eitherMatch (string2cellId <$> R.str)
