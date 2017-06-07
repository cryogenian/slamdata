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

module Test.StrongCheck.Data.String where

import SlamData.Prelude

import Data.Array as Arr
import Data.String as Str

import Test.StrongCheck.Gen as Gen


alphaString ∷ Gen.Gen String
alphaString = do
  length ← Gen.chooseInt 1 100
  Arr.foldRecM foldFn "" $ Arr.range 0 length
  where
  foldFn acc _ = do
    letter ←
      Gen.elements "a"
      $ foldMap (pure ∘ Str.fromCharArray ∘ pure)
      $ Str.toCharArray "bcdefghijklmnopqrstuvwxyz"
    pure $ acc <> letter
