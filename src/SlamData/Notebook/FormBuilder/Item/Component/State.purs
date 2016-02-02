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

module SlamData.Notebook.FormBuilder.Item.Component.State
  ( State()
  , _model
  , initialState
  , module SlamData.Notebook.FormBuilder.Item.Model
  ) where

import Prelude

import Data.Lens

import SlamData.Notebook.FormBuilder.Item.Model

type State =
  { model :: Model
  }

_model :: LensP State Model
_model = lens _.model _ { model = _ }

initialState :: State
initialState =
  { model : initialModel
  }
