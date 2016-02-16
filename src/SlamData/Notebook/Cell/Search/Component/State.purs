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

module SlamData.Notebook.Cell.Search.Component.State
  ( State()
  , initialState
  , _searchString
  , _running
  , StateP()
  ) where

import Prelude

import Data.Lens (LensP(), lens)

import Halogen (InstalledState())

import SlamData.Notebook.Cell.Search.Component.Query as SQ
import SlamData.Effects (Slam())
import SlamData.Notebook.FileInput.Component as FI

type State =
  { searchString :: String
  , running :: Boolean
  }

initialState :: State
initialState =
  { searchString: ""
  , running: false
  }

_searchString :: LensP State String
_searchString = lens _.searchString (_ { searchString = _ })

_running :: LensP State Boolean
_running = lens _.running (_ { running = _ })

type StateP = InstalledState State FI.State SQ.Query FI.Query Slam Unit
