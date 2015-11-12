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

module Notebook.Cell.Common.EvalQuery
  ( CellEvalQuery(..)
  , CellEvalResult()
  ) where

import Data.Either (Either())
import Data.Maybe (Maybe())
import Model.Port (Port())

data CellEvalQuery a
  = EvalCell (Maybe Port) (CellEvalResult -> a)
  | NotifyRunCell a

-- | The result value produced when evaluating a cell.
-- |
-- | - `output` is the value that this cell component produces that is taken as
-- |   the input for dependant cells. Not every cell produces an output.
-- | - `messages` is for any error or status messages that arise during
-- |   evaluation. `Left` values are errors, `Right` values are informational
-- |   messages.
type CellEvalResult =
  { output :: Maybe Port
  , messages :: Array (Either String String)
  }
