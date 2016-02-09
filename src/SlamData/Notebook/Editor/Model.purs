{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook.Editor.Model where

import Prelude

import Control.Bind ((=<<), (>=>))
import Control.Monad.Error.Class (throwError)

import Data.Argonaut (Json(), (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Either (Either(..))
import Data.Map (Map(), empty)
import Data.Traversable (traverse)

import SlamData.Notebook.Cell.CellId (CellId())
import SlamData.Notebook.Cell.Model as Cell

type Notebook =
  { cells :: Array Cell.Model
  , dependencies :: Map CellId CellId
  }

emptyNotebook :: Notebook
emptyNotebook = { cells: [ ], dependencies: empty }

encode :: Notebook -> Json
encode r
   = "version" := 2
  ~> "cells" := map Cell.encode r.cells
  ~> "dependencies" := r.dependencies
  ~> jsonEmptyObject

decode :: Json -> Either String Notebook
decode = decodeJson >=> \obj -> do
  case obj .? "version" of
    Right n | n /= 2 -> throwError "Expected notebook format v2"
    l -> l
  { cells: _, dependencies: _ }
    <$> (traverse Cell.decode =<< obj .? "cells")
    <*> obj .? "dependencies"
