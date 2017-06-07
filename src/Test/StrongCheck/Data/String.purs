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
