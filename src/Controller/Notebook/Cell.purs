module Controller.Notebook.Cell where

import Control.Plus 
import Control.Monad.Eff.Class
import Control.Monad.Aff.Class
import Data.Either (Either(..), either)
import Input.Notebook (Input(..))
import Halogen.HTML.Events.Monad (Event(), async, andThen)
import Data.Date (now, Now())
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _content, _search)
import Optic.Core ((^.), (.~), (..))
import Data.Inject1 (inj)
import EffectTypes
import Model.Notebook.Search (needFields, queryToSQL)
import Text.SlamSearch (mkQuery)
import Api.Query (fields, query)
import Model.Resource (newFile, _path, AnyPath(), Resource())
import Data.Path.Pathy 


import Debug.Foreign

type I e = Event (NotebookAppEff e) Input 

runCellEvent :: forall eff. Cell -> I eff
runCellEvent cell = do 
  ((inj <<< RunCell (cell ^. _cellId)) <$> liftEff now) `andThen` \_ ->
    case cell ^. _content of
      Search s -> runSearch cell
      _ -> empty


tstFile :: Resource
tstFile = newFile # _path .~ (Left $ rootDir </> dir "foo" </> dir "foo" </> file "aabbc")

runSearch :: forall eff. Cell -> I eff
runSearch input =
  either errored go $ mkQuery (input ^. _content .. _search)
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
    



