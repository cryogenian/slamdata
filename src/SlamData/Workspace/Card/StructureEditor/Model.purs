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

module SlamData.Workspace.Card.StructureEditor.Model where

import SlamData.Prelude

import Data.List (List(..))
import Data.Argonaut as J
import SlamData.Workspace.Card.StructureEditor.Common as SEC
import Test.StrongCheck.Gen as SC

newtype Model
  = Model
  { view :: List SEC.ColumnItem
  }

derive instance eqModel ∷ Eq Model
derive instance ordModel ∷ Ord Model
derive instance newtypeModel ∷ Newtype Model _

initialModel ∷ Model
initialModel =
  Model
    { view: Nil
    }

genModel ∷ SC.Gen Model
genModel = pure initialModel

encode ∷ Model → J.Json
encode _ = J.jsonEmptyObject

decode ∷ J.Json → Either String Model
decode _ = pure initialModel
