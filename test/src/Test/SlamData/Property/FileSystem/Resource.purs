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

module Test.SlamData.Property.FileSystem.Resource where

import Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (Either(..))

import SlamData.FileSystem.Resource (Resource)

import Test.StrongCheck (SC, Result(..), quickCheck)

check :: forall eff. SC eff Unit
check = quickCheck \(res ∷ Resource) ->
  case decodeJson (encodeJson res) of
    Left err -> Failed $ "Decode failed: " <> err
    Right res'
      | res == res' -> Success
      | otherwise ->
          Failed
            $ "Decoded resource " <> show res' <> " does not match encoded resource " <> show res
            <> "\n\tEncoded res: " <> show (encodeJson res)
            <> "\n\tEncoded res': " <> show (encodeJson res')
            <> "\nIf you see this, please tell Gary!"
