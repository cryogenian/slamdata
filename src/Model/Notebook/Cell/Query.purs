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

module Model.Notebook.Cell.Query
  ( QueryRec(..)
  , initialQueryRec
  , _input
  , _table
  ) where

import Prelude
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Model.Notebook.Cell.JTableContent (JTableContent(), initialJTableContent)
import Optic.Core 

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
