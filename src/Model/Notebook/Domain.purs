module Model.Notebook.Domain where

import Data.Array (length, sort, reverse, head)
import Data.Tuple
import Data.Maybe
import Data.Map
import Data.Argonaut.Combinators
import qualified Data.Argonaut.Core as Ac
import qualified Data.Argonaut.Decode as Ad
import qualified Data.Argonaut.Encode as Ae
import qualified Data.Argonaut.Printer as Ap
import qualified Network.HTTP.Affjax.Request as Ar

import Optic.Core (lens, LensP(), (..), (.~), (^.))
import Model.Notebook.Cell (CellContent(..), CellId(), Cell(), _cellId, _output, newCell)
import Model.Notebook.Port (Port(..))
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

addCell :: CellContent -> Maybe CellId -> Notebook -> Tuple Notebook Cell
addCell content mbCellId (Notebook n) = Tuple notebook cell
  where
  newId = maybe 0 (+ 1) $ head $ reverse $ sort $ (^. _cellId) <$> n.cells
  cell = newCell newId content # _output .~ port 
  newDeps = maybe n.dependencies (\x -> insert newId x n.dependencies) mbCellId
  notebook = Notebook $ n { cells = cell : n.cells
                          , dependencies = newDeps
                          , activeCellId = newId
                          }
  port = case content of
    Search _ -> PortResource (n.resource `child` ("out" <> show newId))
    _ -> Closed



instance notebookEncode :: Ae.EncodeJson Notebook where
  encodeJson (Notebook {metadata: metadata, cells: cells})
    =  "metadata" := metadata
    ~> "cells" := cells
    ~> Ac.jsonEmptyObject


instance notebookRequestable :: Ar.Requestable Notebook where
  toRequest notebook =
    let str = Ap.printJson (Ae.encodeJson notebook) :: String
    in Ar.toRequest str
