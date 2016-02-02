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

module SlamData.Notebook.Cell.Explore.Component.State where

import Prelude

import Halogen

import SlamData.Notebook.Cell.Common.EvalQuery as NC
import SlamData.Notebook.Effects (Slam())
import SlamData.Notebook.FileInput.Component as FI

type State = Unit

initialState :: State
initialState = unit

type StateP = InstalledState State FI.State NC.CellEvalQuery FI.Query Slam Unit
