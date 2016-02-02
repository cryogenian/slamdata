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

module SlamData.Notebook.Cell.Search.Model
  ( Model()
  , encode
  , decode
  ) where

import Prelude

import Control.Bind ((>=>))

import Data.Argonaut (Json(), (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Either (Either())
import Data.Maybe (Maybe())

import SlamData.FileSystem.Resource (Resource())

type Model =
  { file :: Maybe Resource
  , input :: String
  }

encode :: Model -> Json
encode m
   = "file" := m.file
  ~> "input" := m.input
  ~> jsonEmptyObject

decode :: Json -> Either String Model
decode = decodeJson >=> \obj ->
  { file: _, input: _ }
    <$> obj .? "file"
    <*> obj .? "input"
