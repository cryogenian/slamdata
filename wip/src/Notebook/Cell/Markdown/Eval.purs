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

module Notebook.Cell.Markdown.Eval where

import Prelude

import Data.Maybe (Maybe(..))

import Text.Markdown.SlamDown.Parser (parseMd)

import Notebook.Cell.Common.EvalQuery (CellEvalResult())
import Notebook.Cell.Port (Port(..))

markdownEval :: String -> CellEvalResult
markdownEval s =
  { messages: []
  , output: Just $ SlamDown (parseMd s)
  }
