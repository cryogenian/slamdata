module Model.Notebook where

import Data.Either
import Data.List
import qualified Data.String as Str

import Data.Argonaut.Combinators
import qualified Data.Argonaut.Core as Ac 
import qualified Data.Argonaut.Decode as Ad
import qualified Data.Argonaut.Encode as Ae
import qualified Data.Argonaut.Printer as Ap
import qualified Network.HTTP.Affjax.Request as Ar


import Model.Resume

data Input
  = ViewNotebook String (List CellState)
  | EditNotebook String (List CellState)
  | ViewCell CellState
  | EditCell CellState
    
data Request = Request

data State
  = OneCellView Resume CellState
  | NotebookView Resume String (List CellState)


type CellState = {id :: String}

initialCell :: CellState
initialCell = {id: ""}

initialState :: State
initialState = NotebookView View "" Nil

type CellId = String


string2cellId :: String -> Either String CellId
string2cellId "" = Left "incorrect cellid"
string2cellId a = Right a 

data CellType
  = Evaluate
  | Explore
  | Search
  | Query
  | Visualize
  | Markdown

newtype Cell = Cell {
  input :: String,
  output :: String,
  cellType :: CellType,
  metadata :: String
  }

newtype Notebook = Notebook {
  metadata :: String,
  cells :: [Cell]
  }
                   
newNotebook :: Notebook
newNotebook = Notebook {
  metadata: "",
  cells: []
  }

instance cellTypeEncode :: Ae.EncodeJson CellType where
  encodeJson cell = Ac.fromString $ case cell of
    Evaluate -> "evaluate"
    Explore -> "explore"
    Search -> "search"
    Query -> "query"
    Visualize -> "visualize"
    Markdown -> "markdown" 

instance cellEncode :: Ae.EncodeJson Cell where
  encodeJson (Cell cell) 
    =  "input" := cell.input
    ~> "output" := cell.output
    ~> "cellType" := cell.cellType
    ~> "metadata" := cell.metadata
    ~> Ac.jsonEmptyObject

instance notebookEncode :: Ae.EncodeJson Notebook where
  encodeJson (Notebook {metadata: metadata, cells: cells}) 
    =  "metadata" := metadata
    ~> "cells" := cells
    ~> Ac.jsonEmptyObject 


instance notebookRequestable :: Ar.Requestable Notebook where
  toRequest notebook =
    let str = Ap.printJson (Ae.encodeJson notebook) :: String
    in Ar.toRequest str
