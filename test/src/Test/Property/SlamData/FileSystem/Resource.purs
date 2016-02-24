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

module Test.Property.SlamData.FileSystem.Resource where

import Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (Either(..))
import Data.List (toList)

import SlamData.FileSystem.Resource (Resource(..), Mount(..))

import Test.StrongCheck (QC(), Result(..), Arbitrary, arbitrary, quickCheck, (<?>))
import Test.StrongCheck.Gen (elements)
import Test.Property.Utils.Path (runArbFilePath, runArbDirPath)

newtype ArbResource = ArbResource Resource

runArbResource :: ArbResource -> Resource
runArbResource (ArbResource r) = r

instance arbitraryArbResource :: Arbitrary ArbResource where
  arbitrary = do
    fp <- runArbFilePath <$> arbitrary
    dp <- runArbDirPath <$> arbitrary
    ArbResource <$>
      elements
        (File fp)
        (toList
          [ Mount (View fp)
          , Notebook dp
          , Directory dp
          , Mount (Database dp)
          ])

check :: QC Unit
check = quickCheck $ runArbResource >>> \res ->
  case decodeJson (encodeJson res) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right res' -> res == res' <?> "Decoded resource does not match encoded resource"
