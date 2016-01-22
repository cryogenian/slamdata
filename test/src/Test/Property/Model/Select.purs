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

module Test.Property.Model.Select where

import Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (Either(..))

import Model.Select (Select(..))

import Test.StrongCheck (QC(), Result(..), Arbitrary, arbitrary, quickCheck, (<?>))

newtype ArbSelect a = ArbSelect (Select a)

runArbSelect :: forall a. ArbSelect a -> Select a
runArbSelect (ArbSelect r) = r

instance arbitraryArbSelect :: (Arbitrary a) => Arbitrary (ArbSelect a) where
  arbitrary = do
    options <- arbitrary
    value <- arbitrary
    pure $ ArbSelect $ Select { options, value }

check :: QC Unit
check = quickCheck $ runArbSelect >>> \(s :: Select Int) ->
  case decodeJson (encodeJson s) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right s' -> s == s' <?> "Select failed to decode as encoded value"
