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

module SlamData.Notebook.Cell.JTable.Model
  ( Model()
  , Result()
  , encode
  , decode
  ) where

import Prelude

import Control.Bind ((=<<))

import Data.Argonaut (Json(), (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Either (Either())
import Data.Maybe (Maybe())
import Data.Traversable (traverse)

import SlamData.FileSystem.Resource (Resource())

type Model =
  { input :: Maybe Resource
  , result :: Maybe Result
  }

type Result =
  { json :: Json
  , page :: Int
  , pageSize :: Int
  }

encode :: Model -> Json
encode m
   = "input" := m.input
  ~> "result" := (encodeResult <$> m.result)
  ~> jsonEmptyObject

decode :: Json -> Either String Model
decode json = do
  obj <- decodeJson json
  { input: _, result: _ }
    <$> obj .? "input"
    <*> (traverse decodeResult =<< obj .? "result")

encodeResult :: Result -> Json
encodeResult r
   = "json" := r.json
  ~> "page" := r.page
  ~> "pageSize" := r.pageSize
  ~> jsonEmptyObject

decodeResult :: Json -> Either String Result
decodeResult json = do
  obj <- decodeJson json
  { json: _, page: _, pageSize: _ }
    <$> obj .? "json"
    <*> obj .? "page"
    <*> obj .? "pageSize"
