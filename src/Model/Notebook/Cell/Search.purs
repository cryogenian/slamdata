module Model.Notebook.Cell.Search
  ( SearchRec(..)
  , initialSearchRec
  , _input
  , _table
  , _buffer
  ) where

import Prelude
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Model.Notebook.Cell.FileInput (FileInput(), initialFileInput)
import Model.Notebook.Cell.JTableContent (JTableContent(), initialJTableContent)
import Optic.Core 

import qualified Model.Notebook.Cell.Common as C

newtype SearchRec =
  SearchRec { input :: FileInput
            , table :: JTableContent
            , buffer :: String
            }

initialSearchRec :: SearchRec
initialSearchRec =
  SearchRec { input: initialFileInput
            , table: initialJTableContent
            , buffer: ""
            }

_SearchRec :: LensP SearchRec _
_SearchRec = lens (\(SearchRec obj) -> obj) (const SearchRec)

_input :: LensP SearchRec FileInput
_input = _SearchRec <<< C._input

_table :: LensP SearchRec JTableContent
_table = _SearchRec <<< C._table

_buffer :: LensP SearchRec String
_buffer = _SearchRec <<< lens _.buffer _{buffer = _}

instance encodeJsonQueryRec :: EncodeJson SearchRec where
  encodeJson (SearchRec rec)
    =  "input" := rec.input
    ~> "table" := rec.table
    ~> "buffer" := rec.buffer
    ~> jsonEmptyObject

instance decodeJsonQueryRec :: DecodeJson SearchRec where
  decodeJson json = do
    obj <- decodeJson json
    rec <- { input: _, table: _, buffer: _ }
        <$> obj .? "input"
        <*> obj .? "table"
        <*> obj .? "buffer"
    return $ SearchRec rec
