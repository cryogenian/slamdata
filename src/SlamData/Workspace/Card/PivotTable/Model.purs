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

module SlamData.Workspace.Card.PivotTable.Model where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (~>), (:=), (.?), jsonEmptyObject)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type Model =
  { pageSize ∷ Int
  }

initialModel ∷ Model
initialModel =
  { pageSize: 25
  }

eqModel ∷ Model → Model → Boolean
eqModel m1 m2 = m1.pageSize ≡ m2.pageSize

genModel ∷ Gen.Gen Model
genModel = { pageSize: _ } <$> arbitrary

encode ∷ Model → Json
encode m =
  "pageSize" := m.pageSize
  ~> jsonEmptyObject

decode ∷ Json → Either String Model
decode json = do
  obj ← decodeJson json
  pageSize ← obj .? "pageSize"
  pure { pageSize }
