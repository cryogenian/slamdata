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

module SlamData.Workspace.Card.Ace.Model
  ( Model
  , emptyModel
  , encode
  , decode
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, encodeJson, jsonEmptyObject)

import Utils.Ace (RangeRec, encodeRangeRec, decodeRangeRec)

type Model =
  { text :: String
  , ranges :: Array RangeRec
  }

emptyModel :: Model
emptyModel =
  { text: ""
  , ranges: []
  }

encode :: Model -> Json
encode m
   = ("text" := m.text)
  ~> ("ranges" := (encodeJson $ map encodeRangeRec m.ranges))
  ~> jsonEmptyObject


decode :: Json -> Either String Model
decode = decodeJson >=> \obj -> do
  { text: _, ranges: _ }
    <$> (obj .? "text")
    <*> (obj .? "ranges" >>= traverse decodeRangeRec)
