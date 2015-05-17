module Controller.Notebook.Cell.Explore (runExplore) where

import Control.Monad.Eff.Class (liftEff)
import Controller.Notebook.Cell.JTableContent (runJTable)
import Controller.Notebook.Common (I())
import Data.Date (now)
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..))
import Input.Notebook (Input(CellResult))
import Model.Notebook.Port (_PortResource)
import Model.Notebook.Cell (Cell(), _FileInput, _content, _cellId, _input)
import Model.Notebook.Cell.FileInput (_file)
import Optic.Core ((^.), (..))
import Optic.Fold ((^?))
import Optic.Refractor.Prism (_Just)

import qualified Data.Array.NonEmpty as NEL
import qualified Data.Int as I

runExplore :: forall e. Cell -> I e
runExplore cell = case cell ^? _input .. _PortResource of 
  Just file -> runJTable file cell
  Nothing -> do
    now' <- liftEff now
    return $ inj $ CellResult (cell ^. _cellId) now' (Left $ NEL.singleton "Please select a file")
