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

module SlamData.Workspace.Card.Table.Model
  ( Model
  , emptyModel
  , genModel
  , eqModel
  , codec
  ) where

import SlamData.Prelude

import Data.Codec.Argonaut.Compat as CA
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

type Model =
  { page ∷ Maybe Int
  , pageSize ∷ Maybe Int
  }

eqModel ∷ Model → Model → Boolean
eqModel m1 m2 =
  m1.page ≡ m2.page
    && m1.pageSize ≡ m2.pageSize

genModel ∷ Gen.Gen Model
genModel = do
  page ← SC.arbitrary
  pageSize ← SC.arbitrary
  pure { page, pageSize }

emptyModel ∷ Model
emptyModel =
  { page: Nothing
  , pageSize: Nothing
  }

codec ∷ CA.JsonCodec Model
codec = CA.object "Table model" $ CA.record
  # CA.recordProp (SProxy ∷ SProxy "page") (CA.maybe CA.int)
  # CA.recordProp (SProxy ∷ SProxy "pageSize") (CA.maybe CA.int)
