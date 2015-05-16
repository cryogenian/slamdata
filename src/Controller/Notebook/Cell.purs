module Controller.Notebook.Cell where

import Api.Query (fields, query)
import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import Control.Plus
import Controller.Notebook.Cell.Explore (runExplore)
import Controller.Notebook.Common
import Data.Date (now, Now())
import Data.Either (Either(..), either)
import Data.Inject1 (inj)
import Data.Path.Pathy
import Debug.Foreign
import Halogen.HTML.Events.Monad (Event(), async, andThen)
import Input.Notebook (Input(..))
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _content, _Search)
import Model.Notebook.Search (needFields, queryToSQL)
import Model.Resource (newFile, _path, AnyPath(), Resource())
import Optic.Core ((^.), (.~), (..))
import Text.SlamSearch (mkQuery)

runCellEvent :: forall eff. Cell -> I eff
runCellEvent cell = do
  ((inj <<< RunCell (cell ^. _cellId)) <$> liftEff now) `andThen` \_ ->
    case cell ^. _content of
      Search _ -> runSearch cell
      Explore _ -> runExplore cell
      _ -> empty


tstFile :: Resource
tstFile = newFile # _path .~ (Left $ rootDir </> dir "foo" </> dir "foo" </> file "aabbc")

runSearch :: forall eff. Cell -> I eff
runSearch input =
  either errored go $ mkQuery (input ^. _content .. _Search)
  where
  errored :: _ -> I eff
  errored _ = empty

  go :: _ -> I eff
  go q = do
    fs <- if needFields q
          then liftAff $ fields tstFile
          else pure []
    liftEff $ fprint $ queryToSQL fs q
    res <- liftAff $ query tstFile (queryToSQL fs q)
    liftEff $ fprint $ res
    empty
