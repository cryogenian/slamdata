{-
Copyright 2017 SlamData, Inc.

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

module Test.SlamData.Property.Workspace.Card.StructureEditor.Model where

import SlamData.Prelude

import SlamData.Workspace.Card.StructureEditor.Model as SEM
import Test.StrongCheck (SC, Result(..), quickCheck, (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary)

newtype SCModel = SCModel SEM.Model

derive instance newtypeSCModel :: Newtype SCModel _

instance arbitrarySCModel :: Arbitrary SCModel where
  arbitrary = SCModel <$> SEM.genModel

check :: forall eff. SC eff Unit
check = quickCheck \(SCModel model) ->
  case SEM.decode (SEM.encode model) of
    Left err -> Failed $ "Decode failed: " <> err
    Right model' ->
      model == model' <?> "Model failed to decode as encoded value"
