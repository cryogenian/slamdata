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

module SlamData.Workspace.Card.Chart.Model where

import SlamData.Prelude
import Data.Argonaut (Json, decodeJson, (~>), (:=), (.?), jsonEmptyObject)
import Test.StrongCheck.Gen as Gen

type Model = Unit

eqModel ∷ Model → Model → Boolean
eqModel = eq

genModel ∷ Gen.Gen Model
genModel = pure unit

emptyModel ∷ Model
emptyModel = unit

encode ∷ Model → Json
encode _ =
  "tag" := "chart"
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode = decodeJson >=> \obj → do
  tag ← obj .? "tag"
  unless (tag ≡ "chart") $ Left "This is not a chart model"
  pure unit
