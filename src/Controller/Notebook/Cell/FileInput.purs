module Controller.Notebook.Cell.FileInput
  ( toggleFileList
  , selectFile
  , updateFile
  ) where

import Controller.Common (getFiles)
import Controller.Notebook.Common
import Control.Plus (empty)
import Data.Array (sort, nub)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Path.Pathy ((</>), parseAbsFile, sandbox, rootDir)
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(UpdateCell))
import Model.Notebook (State(), _notebook)
import Model.Notebook.Cell (Cell(), CellId(), _FileInput, _content, _cellId, _input)
import Model.Notebook.Cell.FileInput (FileInput(), _files, _showFiles, _file)
import Model.Notebook.Domain (_cells)
import Model.Notebook.Port (Port(..))
import Model.Resource (Resource(), newFile, root, _path)
import Optic.Core ((^.), (.~), (%~), (?~), (..), (+~))
import Optic.Extended (TraversalP(), (^?))
import Optic.Index (ix)

toggleFileList :: forall e. Cell -> I e
toggleFileList cell =
  let showing = fromMaybe true (cell ^? _lens .. _showFiles)
      upd = pure $ UpdateCell (cell ^. _cellId) (_lens .. _showFiles .~ not showing)
  in if showing then upd else upd `andThen` \_ -> populateFiles cell

populateFiles :: forall e. Cell -> I e
populateFiles cell =
  getFiles (\newFiles -> pure $ UpdateCell (cell ^. _cellId) (_lens .. _files %~ updateFiles newFiles)) root
  where
  updateFiles :: [Resource] -> [Resource] -> [Resource]
  updateFiles newFiles oldFiles = sort $ nub $ (oldFiles ++ newFiles)

selectFile :: forall e. Cell -> Resource -> I e
selectFile cell res =
  pure $ UpdateCell (cell ^. _cellId) $ (_lens .. _showFiles .~ false)
                                     .. (_lens .. _file .~ Right res)
                                     .. (_input .~ PortResource res)

updateFile :: forall e. Cell -> String -> I e
updateFile cell path =
  case (rootDir </>) <$> (parseAbsFile path >>= sandbox rootDir) of
    Just path' ->
      let res = newFile # _path .~ inj path'
      in pure $ UpdateCell (cell ^. _cellId) $ (_lens .. _file .~ Right res)
                                            .. (_input .~ PortResource res)
    Nothing -> pure $ UpdateCell (cell ^. _cellId) $ (_lens .. _file .~ Left path)
                                                  .. (_input .~ PortInvalid "Please enter a valid file path")

_lens :: TraversalP Cell FileInput
_lens = _content .. _FileInput
