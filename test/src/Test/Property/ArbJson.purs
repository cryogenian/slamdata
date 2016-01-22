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

module Test.Property.ArbJson
  ( ArbJson()
  , runArbJson
  , ArbJCursor()
  , runArbJCursor
  ) where

import Prelude

import Control.Lazy (defer, fix)

import Data.Argonaut
import Data.Array as A
import Data.String as S

import Test.StrongCheck
import Test.StrongCheck.Gen

newtype ArbJson = ArbJson Json

runArbJson :: ArbJson -> Json
runArbJson (ArbJson m) = m

instance arbitraryArbResult :: Arbitrary ArbJson where
  arbitrary = ArbJson <$> fix gen
    where
    gen :: Gen Json -> Gen Json
    gen genJson =
      oneOf
        (pure jsonNull)
        [ fromBoolean <$> arbitrary
        , fromNumber <$> arbitrary
        , fromString <$> arbitrary
        , defer \_ -> genArray genJson
        , defer \_ -> genObject genJson
        ]

genArray :: Gen Json -> Gen Json
genArray genJson = do
  numItems <- chooseInt 0.0 2.0
  fromArray <$> vectorOf numItems genJson

genObject :: Gen Json -> Gen Json
genObject genJson = do
  numKeys <- chooseInt 0.0 2.0
  keys <- vectorOf numKeys (suchThat arbitrary (not <<< S.null))
  A.foldM extendObj jsonEmptyObject keys
  where
  extendObj :: Json -> String -> Gen Json
  extendObj obj k = do
    val <- genJson
    pure $ k := val ~> obj

newtype ArbJCursor = ArbJCursor JCursor

runArbJCursor :: ArbJCursor -> JCursor
runArbJCursor (ArbJCursor j) = j

instance arbJCursor :: Arbitrary ArbJCursor where
  arbitrary = do
    i <- chooseInt 0.0 2.0
    r <- if i == 0 then pure JCursorTop
         else if i == 1 then JField <$> arbitrary <*> (runArbJCursor <$> arbitrary)
              else JIndex <$> arbitrary <*> (runArbJCursor <$> arbitrary)
    pure $ ArbJCursor r
