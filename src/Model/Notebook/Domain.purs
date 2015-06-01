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
  , deps
  , addCell
  , insertCell
  , notebookPath
  , notebookURL
  , cellURL
  , cellOut
  ) where

import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Array (length, sort, reverse, head, insertAt, findIndex, elemIndex, snoc)
import Data.Either (Either(..))
import Data.Map (Map(), insert, lookup, empty, toList, fromList)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Path.Pathy (rootDir, dir, file, (</>), printPath)
import Data.These (These(..), these, theseRight)
import Data.Tuple (Tuple(..))
import Model.Notebook.Cell (CellContent(..), CellId(), Cell(), _cellId, _output, newCell, _input, _parent, _FileInput)
import Model.Notebook.Cell.FileInput (_file, fileFromString, portFromFile)
import Model.Notebook.Port (Port(..), _PortResource)
import Model.Action (Action(), printAction)
import Model.Path (DirPath(), (<./>), encodeURIPath)
import Model.Resource (Resource(File))
import Network.HTTP.Affjax.Request (Requestable, toRequest)
import Optic.Core (lens, LensP(), (..), (.~), (^.))
import Optic.Extended (TraversalP())
import Optic.Fold ((^?))
import Optic.Index (ix)

import qualified Data.StrMap as SM

newtype Notebook =
  Notebook { cells :: [Cell]
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

_cells :: LensP Notebook [Cell]
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
  let i = findIndex (\cell -> cell ^. _cellId == cellId) (s ^. _cells)
  in (_cells .. ix i) f s

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

deps :: CellId -> Dependencies -> [CellId]
deps cid ds = deps' cid ds []
  where
  deps' cid ds agg =
    case lookup cid ds of
      Nothing -> agg
      Just a -> deps' a ds (a : agg)

addCell :: CellContent -> Notebook -> Tuple Notebook Cell
addCell content oldNotebook@(Notebook n) = Tuple notebook cell
  where
  newId = freeId oldNotebook
  cell = newCell newId content # (_output .~ cellOut content oldNotebook newId)
  notebook = Notebook $ n { cells = n.cells `snoc` cell
                          , activeCellId = newId
                          }

insertCell :: Cell -> CellContent -> Notebook -> Tuple Notebook Cell
insertCell parent content oldNotebook@(Notebook n) = Tuple new cell
  where
  new = Notebook $ n { cells = insertAt (i + 1) cell n.cells
                     , dependencies = newDeps
                     }
  i = elemIndex parent n.cells
  newDeps = insert newId (parent ^. _cellId) n.dependencies
  newId = freeId oldNotebook
  input = parent ^. _output
  cell = newCell newId (content # _FileInput .. _file .~ maybe (Left "") Right (input ^? _PortResource))
         # (_output .~ port)
         ..(_input  .~ input)
         ..(_parent .~ Just (parent ^. _cellId))
  port = cellOut content oldNotebook newId

cellOut :: CellContent -> Notebook -> CellId -> Port
cellOut content n cid = case content of
  (Explore _) -> Closed
  (Visualize _) -> Closed
  (Markdown _) -> VarMap SM.empty
  _ -> these (const Closed) portRes (\_ -> portRes) (n ^. _name)
  where
  portRes :: String -> Port
  portRes _ = maybe Closed (\p -> PortResource $ File $ p </> file ("out" <> show cid)) (notebookPath n)

freeId :: Notebook -> CellId
freeId (Notebook n) =
  maybe 0 (+ 1) $ head $ reverse $ sort $ (^. _cellId) <$> n.cells

notebookPath :: Notebook -> Maybe DirPath
notebookPath nb = (\name -> (nb ^. _path) </> dir name <./> Config.notebookExtension) <$> theseRight (nb ^. _name)

notebookURL :: Notebook -> Action -> Maybe String
notebookURL nb act = (\path -> Config.notebookUrl ++ "#" ++ encodeURIPath (printPath path) ++ printAction act) <$> notebookPath nb

cellURL :: Notebook -> CellId -> Action -> Maybe String
cellURL nb cid act = (\path -> Config.notebookUrl ++ "#" ++ encodeURIPath (printPath path) ++ "cells" ++ "/" ++ show cid ++ "/" ++ printAction act) <$> notebookPath nb
