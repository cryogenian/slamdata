{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Model.Notebook.Domain
  ( Notebook()
  , emptyNotebook
  , _cells
  , _dependencies
  , _activeCellId
  , _name
  , _path
  , _activeCell
  , cellById
  , Dependencies()
  , addCell
  , insertCell
  , notebookPath
  , notebookURL
  , cellURL
  , cellOut
  , replacePendingPorts
  , syncCellsOuts
  , ancestors
  , descendants
  , trash
  ) where

import Prelude
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Array (length, sort, reverse, head, insertAt, findIndex, elemIndex, snoc, filter, (:))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map(), insert, lookup, empty, toList, fromList, delete)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Path.Pathy (rootDir, dir, file, (</>), printPath)
import Data.These (These(..), these, theseRight)
import Data.Tuple (Tuple(..), fst, snd)
import Model.Action (Action(), printAction)
import Model.Notebook.Cell (CellContent(..), CellId(), Cell(), _cellId, _output, newCell, _input, _parent, _FileInput, _content, _pathToNotebook)
import Model.Notebook.Cell.FileInput (_file, fileFromString, portFromFile)
import Model.Notebook.Port (Port(..), _PortResource)
import Model.Path (DirPath(), (<./>), encodeURIPath)
import Model.Resource (Resource(File), _tempFile, _root)
import Network.HTTP.Affjax.Request (Requestable, toRequest)
import Optic.Core
import Optic.Extended (TraversalP())
import Optic.Fold ((^?))
import Optic.Index (ix)
import Optic.Refractor.Prism (_Right)

import qualified Data.StrMap as SM
import qualified Data.List as L

newtype Notebook =
  Notebook { cells :: Array Cell
           , dependencies :: Dependencies
           , activeCellId :: CellId
           , name :: These String String
           , path :: DirPath
           }

emptyNotebook :: Notebook
emptyNotebook = Notebook
  { cells: []
  , dependencies: empty
  , activeCellId: 0
  , name: This Config.newNotebookName
  , path: rootDir
  }

_Notebook :: LensP Notebook _
_Notebook = lens (\(Notebook r) -> r) (\_ r -> Notebook r)

_cells :: LensP Notebook (Array Cell)
_cells = _Notebook .. lens _.cells _{cells = _}

_dependencies :: LensP Notebook Dependencies
_dependencies = _Notebook .. lens _.dependencies _{dependencies = _}

_activeCellId :: LensP Notebook CellId
_activeCellId = _Notebook .. lens _.activeCellId _{activeCellId = _}

_name :: LensP Notebook (These String String)
_name = _Notebook .. lens _.name _{name = _}

_path :: LensP Notebook DirPath
_path = _Notebook .. lens _.path _{path = _}

_activeCell :: TraversalP Notebook Cell
_activeCell f s = cellById (s ^. _activeCellId) f s

cellById :: CellId -> TraversalP Notebook Cell
cellById cellId f s = 
  case findIndex (\cell -> cell ^. _cellId == cellId) (s ^. _cells) of
    Nothing -> pure s
    Just i -> (_cells .. ix i) f s
     

instance encodeJsonNotebook :: EncodeJson Notebook where
  encodeJson (Notebook obj)
    =  "cells" := obj.cells
    ~> "dependencies" := toList obj.dependencies
    ~> jsonEmptyObject

instance decodeJsonNotebook :: DecodeJson Notebook where
  decodeJson json = do
    obj <- decodeJson json
    cells <- obj .? "cells"
    dependencies <- obj .? "dependencies"
    return (emptyNotebook # (_cells .~ cells)
                         .. (_dependencies .~ fromList dependencies))

instance requestableNotebook :: Requestable Notebook where
  toRequest notebook =
    let str = printJson (encodeJson notebook) :: String
    in toRequest str

-- We have no any cells that have more than one
-- dependency right now. And have no ui to make
-- cell that has more than one deps.
type Dependencies = Map CellId CellId

ancestors :: CellId -> Dependencies -> Array CellId
ancestors cid ds = deps' cid ds []
  where
  deps' cid ds agg =
    case lookup cid ds of
      Nothing -> agg
      Just a -> deps' a ds (a : agg)

descendants :: CellId -> Dependencies -> Array CellId
descendants cid ds = L.fromList (fst <$> (L.filter (\x -> snd x == cid) $ toList ds))

trash :: CellId -> Dependencies -> Dependencies
trash cid ds =
  let ds' = delete cid ds 
  in case descendants cid ds' of
    [] -> ds'
    xs -> foldl (\d x -> trash x d) ds' xs

    

addCell :: CellContent -> Notebook -> Tuple Notebook Cell
addCell content oldNotebook = insertCell' Nothing content oldNotebook

insertCell :: Cell -> CellContent -> Notebook -> Tuple Notebook Cell
insertCell parent content oldNotebook = insertCell' (Just parent) content oldNotebook 

insertCell' :: Maybe Cell -> CellContent -> Notebook -> Tuple Notebook Cell
insertCell' mbParent content oldNotebook@(Notebook n) = Tuple new cell
  where
  new = Notebook $ n { cells = fromMaybe (snoc n.cells cell)
                               $ insertAt (i + 1) cell n.cells
                     , dependencies = newDeps
                     }
  i = fromMaybe (length n.cells) $ mbParent >>= flip elemIndex n.cells
  newDeps = maybe n.dependencies (\parent -> insert newId (parent ^. _cellId) n.dependencies) mbParent
  newId = freeId oldNotebook
  input = case content of
    Explore _ -> maybe Closed PortResource (content ^? _FileInput .. _file .. _Right)
    _ -> maybe Closed (^. _output) mbParent
  cell = newCell newId content 
         # (_input .~ input)
         ..(_pathToNotebook .~ (fromMaybe rootDir $ notebookPath oldNotebook))
         ..(_parent .~ ((^. _cellId) <$> mbParent))
         # setPort
  setPort :: Cell -> Cell
  setPort cell = cell # _output .~ (cellOut cell oldNotebook)

cellOut :: Cell -> Notebook -> Port
cellOut cell n = case cell ^._content of
  (Explore _) -> cell ^._input
  (Visualize _) -> Closed
  (Markdown _) -> VarMap SM.empty
  _ -> maybe PortResourcePending (\p -> PortResource $ File $ p </> file ("out" <> show cid)) (notebookPath n)
  where cid = cell ^._cellId

freeId :: Notebook -> CellId
freeId (Notebook n) =
  maybe 0 (+ 1) $ head $ reverse $ sort $ (^. _cellId) <$> n.cells

notebookPath :: Notebook -> Maybe DirPath
notebookPath nb = (\name -> (nb ^. _path) </> dir name <./> Config.notebookExtension) <$> theseRight (nb ^. _name)

notebookURL :: Notebook -> Action -> Maybe String
notebookURL nb act = (\path -> Config.notebookUrl ++ "#" ++ encodeURIPath (printPath path) ++ printAction act) <$> notebookPath nb

cellURL :: Notebook -> CellId -> Action -> Maybe String
cellURL nb cid act = (\path -> Config.notebookUrl ++ "#" ++ encodeURIPath (printPath path) ++ "cells" ++ "/" ++ show cid ++ "/" ++ printAction act) <$> notebookPath nb

replacePendingPorts :: Notebook -> Notebook
replacePendingPorts nb = nb # _cells .. mapped %~ replacePendingPort nb

replacePendingPort :: Notebook -> Cell -> Cell
replacePendingPort nb cell = case cell ^._output of
  PortResourcePending -> cell # _output .~ cellOut cell nb
  _ -> cell

syncCellsOuts :: DirPath -> Notebook -> Notebook
syncCellsOuts path notebook =
  notebook # (_cells..mapped.._input.._PortResource.._tempFile.._root .~ path)
           ..(_cells..mapped.._output.._PortResource.._tempFile.._root .~ path)
           ..(_cells..mapped.._pathToNotebook .~ path)
