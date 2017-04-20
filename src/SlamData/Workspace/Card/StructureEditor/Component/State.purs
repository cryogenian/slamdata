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

module SlamData.Workspace.Card.StructureEditor.Component.State where

import SlamData.Prelude

import Control.Monad.Aff.Future (Future)
import Data.Json.Extended (EJson)
import Data.List as L
import SlamData.Workspace.Card.StructureEditor.Common as SEC
import Utils.Path as PU

type State =
  { cycle ∷ Int
  , resource ∷ Maybe (PU.FilePath × Future (Array EJson))
  , selectedPath ∷ L.List SEC.ColumnItem
  }

initialState ∷ State
initialState =
  { cycle: 0
  , resource: Nothing
  , selectedPath: L.Nil
  }
