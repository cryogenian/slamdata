module Driver.Notebook.Routing where

import Data.Either
import Control.Apply
import Control.Alt
import Model.Path
import Model.Notebook
import Model.Action
import Config (notebookExtension)
import qualified Routing as R
import qualified Routing.Match as R
import qualified Routing.Match.Class as R
import qualified Routing.Hash as R
import qualified Data.String as Str

data Routes
  = CellRoute Path CellId Action
  | NotebookRoute Path Action


routing :: R.Match Routes
routing = CellRoute <$> notebook <*> (R.lit "cells" *> cellId) <*> action
          <|>
          NotebookRoute <$> notebook <*> action
  where notebook = Path <$> (oneSlash *> (R.list notName)) <*> name
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
