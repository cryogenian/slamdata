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

module Input.File
  ( Input()
  , FileInput(..)
  , updateState
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Either (Either())
import Data.Inject1 (prj)
import Data.Maybe.Unsafe (fromJust)
import Input.File.Item (ItemInput(), inputItem)
import Input.File.Mount (MountInput(), inputMount)
import Model.File (State(), _items, _dialog, _sort, isSearching)
import Optic.Core
import Optic.Refractor.Prism (_Just)

type Input = Either ItemInput (Either FileInput MountInput)

data FileInput = WithState (State -> State)

updateState :: State -> Input -> State
updateState state input =
  fromJust $ (inputFile state <$> prj input)
         <|> ((\i -> state # _items %~ inputItem (state ^. _sort) (isSearching state) i) <$> prj input)
         <|> ((\i -> state # _dialog .. _Just %~ inputMount i) <$> prj input)

inputFile :: State -> FileInput -> State
inputFile state (WithState f) = f state
