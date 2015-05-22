module Model.Notebook.Cell.Query
  ( QueryRec(..)
  , initialQueryRec
  , _input
  , _table
  ) where

import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Model.Notebook.Cell.JTableContent (JTableContent(), initialJTableContent)
import Optic.Core (LensP(), lens)

import qualified Model.Notebook.Cell.Common as C

newtype QueryRec =
  QueryRec { input :: String
           , table :: JTableContent
           }

initialQueryRec :: QueryRec
initialQueryRec =
  QueryRec { input: ""
           , table: initialJTableContent
           }

_QueryRec :: LensP QueryRec _
_QueryRec = lens (\(QueryRec obj) -> obj) (const QueryRec)

_input :: LensP QueryRec String
_input = _QueryRec <<< C._input

_table :: LensP QueryRec JTableContent
_table = _QueryRec <<< C._table

instance encodeJsonQueryRec :: EncodeJson QueryRec where
  encodeJson (QueryRec rec)
    =  "input" := rec.input
    ~> "table" := rec.table
    ~> jsonEmptyObject

instance decodeJsonQueryRec :: DecodeJson QueryRec where
  decodeJson json = do
    obj <- decodeJson json
    rec <- { input: _, table: _ }
        <$> obj .? "input"
        <*> obj .? "table"
    return $ QueryRec rec
