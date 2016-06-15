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

module SlamData.Workspace.FormBuilder.Model
  ( Model
  , encode
  , decode
  , emptyModel
  , eqModel
  , genModel
  ) where

import SlamData.Prelude

import Data.Argonaut ((.?), (:=), (~>))
import Data.Argonaut as J

import SlamData.Workspace.FormBuilder.Item.Model as Item
import Test.StrongCheck.Gen as Gen

type Model =
  { items ∷ Array Item.Model
  }

genModel ∷ Gen.Gen Model
genModel = do
  items ← Gen.arrayOf Item.genModel
  pure { items }

eqModel
  ∷ Model
  → Model
  → Boolean
eqModel m1 m2 =
  Item.EqModel <$> m1.items ≡ Item.EqModel <$> m2.items

emptyModel ∷ Model
emptyModel = { items: [] }

encode
  ∷ Model
  → J.Json
encode m =
  "items" := (Item.encode <$> m.items)
     ~> J.jsonEmptyObject

decode
  ∷ J.Json
  → Either String Model
decode =
  J.decodeJson >=> \obj →
    obj .? "items"
      >>= traverse Item.decode
      <#> { items : _ }
