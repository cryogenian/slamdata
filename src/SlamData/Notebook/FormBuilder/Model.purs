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

module SlamData.Notebook.FormBuilder.Model
  ( Model()
  , encode
  , decode
  ) where

import Prelude

import Control.Bind ((>=>))

import Data.Argonaut ((.?), (:=), (~>))
import Data.Argonaut as J
import Data.Either as E
import Data.Traversable as Tr

import SlamData.Notebook.FormBuilder.Item.Model as Item

type Model =
  { items :: Array Item.Model
  }

encode
  :: Model
  -> J.Json
encode m =
  "items" := (Item.encode <$> m.items)
     ~> J.jsonEmptyObject

decode
  :: J.Json
  -> E.Either String Model
decode =
  J.decodeJson >=> \obj ->
    obj .? "items"
      >>= Tr.traverse Item.decode
      <#> { items : _ }
