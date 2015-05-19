module Controller.Notebook.Cell.FileInput
  ( toggleFileList
  , selectFile
  , updateFile
  ) where

import Controller.Common (getFiles)
import Controller.Notebook.Common (I())
import Data.Array (sort, nub)
import Data.Either (Either(..), either)
import Data.Maybe (fromMaybe)
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(UpdateCell))
import Model.Notebook.Cell (Cell(), _FileInput, _content, _cellId, _input)
import Model.Notebook.Cell.FileInput (FileInput(), _files, _showFiles, _file, fileFromString, portFromFile)
import Model.Notebook.Port (Port(..))
import Model.Resource (Resource(), root)
import Optic.Core ((^.), (.~), (%~), (..))
import Optic.Extended (TraversalP(), (^?))

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
  let file = fileFromString path
      port = portFromFile file
  in pure $ UpdateCell (cell ^. _cellId) $ (_lens .. _file .~ file)
                                        .. (_input .~ port)

_lens :: TraversalP Cell FileInput
_lens = _content .. _FileInput
