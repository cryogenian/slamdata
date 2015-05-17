module Model.Notebook.Cell.Search where

import Data.Argonaut.Core (JArray())
import Data.Int (Int(), fromNumber)
import Optic.Core (Lens(), lens) 

import Model.Notebook.Cell.FileInput (FileInput(), initialFileInput)
import Model.Notebook.Cell.JTableContent (JTableContent(), initialJTableContent)

type SearchRec =
  { input :: FileInput
  , table :: JTableContent
  , buffer :: String
  }
  
initialSearchRec :: SearchRec
initialSearchRec =
  { input: initialFileInput
  , table: initialJTableContent 
  , buffer: ""
  }

_buffer :: forall a b r. Lens {buffer :: a | r} {buffer :: b | r} a b
_buffer = lens _.buffer _{buffer = _}


