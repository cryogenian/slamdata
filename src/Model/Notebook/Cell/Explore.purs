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

module Model.Notebook.Cell.Explore
  ( ExploreRec()
  , initialExploreRec
  , _input
  , _table
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

newtype ExploreRec =
  ExploreRec { input :: FileInput
             , table :: JTableContent
             }

initialExploreRec :: ExploreRec
initialExploreRec =
  ExploreRec { input: initialFileInput
             , table: initialJTableContent
             }

_ExploreRec :: LensP ExploreRec _
_ExploreRec = lens (\(ExploreRec obj) -> obj) (const ExploreRec)

_input :: LensP ExploreRec FileInput
_input = _ExploreRec <<< C._input

_table :: LensP ExploreRec JTableContent
_table = _ExploreRec <<< C._table

instance encodeJsonExploreRec :: EncodeJson ExploreRec where
  encodeJson (ExploreRec rec)
    =  "input" := rec.input
    ~> "table" := rec.table
    ~> jsonEmptyObject

instance decodeJsonExploreRec :: DecodeJson ExploreRec where
  decodeJson json = do
    obj <- decodeJson json
    rec <- { input: _, table: _ }
        <$> obj .? "input"
        <*> obj .? "table"
    return $ ExploreRec rec
