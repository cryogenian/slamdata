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

module SlamData.Workspace.Card.ChartOptions.Model
  ( Model
  , eqModel
  , encode
  , decode
  , initialModel
  , genModel
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, (.?), encodeJson, decodeJson, (~>), (:=))

import SlamData.Workspace.Card.Chart.Config as CC
import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type Model = Maybe CC.ChartConfig

eqModel ∷ Model → Model → Boolean
eqModel = eq

genModel ∷ Gen.Gen Model
genModel = arbitrary

encode ∷ Model → Json
encode = encodeJson

decode ∷ Json → Either String Model
decode js = (decodeJson js) <|> (pure Nothing)

initialModel ∷ Model
initialModel = Nothing
