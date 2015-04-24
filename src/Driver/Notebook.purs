module Driver.Notebook where

import Data.Either
import Data.List
import Control.Alt
import Control.Apply
import Control.Monad.Eff
import Model.Action

import qualified Utils as U
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Halogen as H
import qualified Routing as R
import qualified Routing.Match as R
import qualified Routing.Match.Class as R
import qualified Routing.Hash as R
import qualified Model.Notebook as M
import qualified Model.Path as M

data Routes
  = Cell M.Path M.CellId Action
  | Notebook M.Path Action

routing :: R.Match Routes
routing = Cell <$> notebook <*> (R.lit "cells" *> cellId) <*> action
          <|>
          Notebook <$> notebook <*> action
  where notebook = M.Path <$> (oneSlash *> (R.list notName)) <*> name
        oneSlash = R.lit ""
        notebookName input =
          if Str.indexOf ".slam" input == -1 then
            Left input
          else Right input
        pathPart input =
          if input == "" || Str.indexOf ".slam" input /= -1 then
            Left "incorrect path part"
          else Right input

        name = R.eitherMatch (notebookName <$> R.str)
        notName = R.eitherMatch (pathPart <$> R.str)

        action = (R.eitherMatch (string2action <$> R.str)) <|> pure View
        cellId = R.eitherMatch (M.string2cellId <$> R.str)

-- Mock
driver :: forall e. H.Driver M.Input e -> Eff (H.HalogenEffects e) Unit
driver k =
  R.matches' M.decodeURIPath routing \old new -> do
    case new of
      Cell path id View -> k $ M.ViewCell (cell path id)
      Cell path id Edit -> k $ M.EditCell (cell path id)
      Notebook path View -> k $ M.ViewNotebook (M.path2str path) (cells path)
      Notebook path Edit -> k $ M.EditNotebook (M.path2str path) (cells path)

  where cell path id = {id: M.path2str path <> ":" <> id}
        cells path =
          (Cons (cell path "3")
           (Cons (cell path "4")
            Nil))


