module Model.Notebook.Domain where

import Data.Array (length, sort, reverse, head, insertAt, findIndex, elemIndex)
import Data.Tuple
import Data.Maybe
import Data.Either (Either(..))
import Data.Map
import Data.Argonaut.Combinators
import qualified Data.Argonaut.Core as Ac
import qualified Data.Argonaut.Decode as Ad
import qualified Data.Argonaut.Encode as Ae
import qualified Data.Argonaut.Printer as Ap
import qualified Network.HTTP.Affjax.Request as Ar

import Optic.Core (lens, LensP(), (..), (.~), (^.))
import Optic.Fold ((^?))
import Model.Notebook.Cell (CellContent(..), CellId(), Cell(), _cellId, _output, newCell, _input, _FileInput)
import Model.Notebook.Cell.FileInput (_file, fileFromString, portFromFile)
import Model.Notebook.Port (Port(..), _PortResource)
import Model.Resource

-- We have no any cells that have more than one
-- dependency right now. And have no ui to make
-- cell that has more than one deps.
type Dependencies = Map CellId CellId
type Metadata = String

deps :: CellId -> Dependencies -> [CellId]
deps cid ds = deps' cid ds []
  where
  deps' cid ds agg =
    case lookup cid ds of
      Nothing -> agg
      Just a -> deps' a ds (a : agg)


type NotebookRec =
  { metadata :: Metadata
  , cells :: [Cell]
  , dependencies :: Dependencies
  , activeCellId :: CellId
  , resource :: Resource
  }

newtype Notebook = Notebook NotebookRec


_notebookRec :: LensP Notebook NotebookRec
_notebookRec = lens (\(Notebook r) -> r) (\_ r -> Notebook r)

_activeCellIdRec :: LensP NotebookRec CellId
_activeCellIdRec = lens _.activeCellId _{activeCellId = _}

_resourceRec :: LensP NotebookRec Resource
_resourceRec = lens _.resource _{resource = _}

_activeCellId :: LensP Notebook CellId
_activeCellId = _notebookRec .. _activeCellIdRec

_resource :: LensP Notebook Resource
_resource = _notebookRec .. _resourceRec

_dependenciesRec :: LensP NotebookRec Dependencies
_dependenciesRec = lens _.dependencies _{dependencies = _}

_dependencies :: LensP Notebook Dependencies
_dependencies = _notebookRec .. _dependenciesRec

_cellsRec :: LensP NotebookRec [Cell]
_cellsRec = lens _.cells _{cells = _}

_cells :: LensP Notebook [Cell]
_cells = _notebookRec .. _cellsRec

_metadataRec :: LensP NotebookRec Metadata
_metadataRec = lens _.metadata _{metadata = _}

_metadata :: LensP Notebook Metadata
_metadata = _notebookRec .. _metadataRec

emptyNotebook :: Notebook
emptyNotebook = Notebook
  { metadata: ""
  , resource: newNotebook
  , cells: []
  , dependencies: empty
  , activeCellId: 0
  }

addCell :: CellContent -> Notebook -> Tuple Notebook Cell
addCell content oldNotebook@(Notebook n) = Tuple notebook cell
  where
  newId = freeId oldNotebook
  cell = newCell newId content # (_output .~ port) .. setInp
  setInp = case content of
    Explore r -> _input .~ port
    _ -> id
  notebook = Notebook $ n { cells = cell : n.cells
                          , activeCellId = newId
                          }
  port = portByContent oldNotebook content newId

insertCell :: Cell -> CellContent -> Notebook -> Tuple Notebook Cell
insertCell parent content oldNotebook@(Notebook n) = Tuple new cell
  where
  new = Notebook $ n { cells = insertAt (i + 1) cell n.cells
                     , dependencies = newDeps
                     }

  i = elemIndex parent n.cells

  newDeps = insert newId (parent ^. _cellId) n.dependencies
  newId = freeId oldNotebook
  cell = newCell newId (content # _FileInput .. _file .~ maybe (Left "") Right (port ^? _PortResource))
         # (_output .~ port) .. (_input .~ (parent ^. _output))
  port = portByContent oldNotebook content newId

portByContent :: Notebook -> CellContent -> CellId -> Port
portByContent (Notebook n) (Search _) cid =
  PortResource (n.resource `child` ("out" <> show cid))
portByContent _ content@(Explore _) _ =
  maybe Closed portFromFile (content ^? _FileInput .. _file)
portByContent _ _ _ = Closed

freeId :: Notebook -> CellId
freeId (Notebook n) =
  maybe 0 (+ 1) $ head $ reverse $ sort $ (^. _cellId) <$> n.cells


instance notebookEncode :: Ae.EncodeJson Notebook where
  encodeJson (Notebook {metadata: metadata, cells: cells})
    =  "metadata" := metadata
    ~> "cells" := cells
    ~> Ac.jsonEmptyObject


instance notebookRequestable :: Ar.Requestable Notebook where
  toRequest notebook =
    let str = Ap.printJson (Ae.encodeJson notebook) :: String
    in Ar.toRequest str
