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

module RectanglePacking where

import SlamData.Prelude

import Data.Array as A
import Data.Int as Int
import Data.Foldable as F
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE

import Math as Math

import Utils.DOM (DOMRect)

type Dimensions = { width ∷ Number, height ∷ Number }

factors ∷ Int → NE.NonEmpty Array Int
factors n = 1 :| do
  factor ← 2 A... n
  guard $ n `mod` factor ≡ 0
  pure factor

goldenRatio ∷ Number
goldenRatio =
  1.61803398875

maybeNotZero ∷ Dimensions → Maybe Dimensions
maybeNotZero dimensions
  | dimensions.width ≡ 0.0 ∨ dimensions.height ≡ 0.0 = Nothing
  | otherwise = Just dimensions

mostRatioFittingRectangle ∷ Number → Int → Dimensions → Dimensions
mostRatioFittingRectangle ratio i boundingDimensions = case tailMax of
  Just m | compareFn hd m ≡ LT → m
  _ → hd
  where
  hd ∷ Dimensions
  hd = NE.head solutions

  tailMax ∷ Maybe Dimensions
  tailMax =
    F.maximumBy compareFn $ NE.tail solutions

  compareFn x y =
    karat x `compare` karat y

  solutions ∷ NE.NonEmpty Array Dimensions
  solutions =
    solution <$> factors i

  karat ∷ Dimensions → Number
  karat dimensions =
    -(Math.abs $ ratio - (dimensions.width / dimensions.height))

  solution ∷ Int → Dimensions
  solution factor =
    { width: boundingDimensions.width / (Int.toNumber $ numberOfRows factor)
    , height: boundingDimensions.height / (Int.toNumber $ numberOfColumns factor)
    }

  numberOfRows ∷ Int → Int
  numberOfRows factor =
    if boundingDimensions.width > boundingDimensions.height
       then factor
       else i / factor

  numberOfColumns ∷ Int → Int
  numberOfColumns factor =
    if boundingDimensions.width > boundingDimensions.height
       then i / factor
       else factor

floor ∷ Dimensions → Dimensions
floor dimensions =
  { width: Math.floor dimensions.width
  , height: Math.floor dimensions.height
}

domRectToDimensions ∷ DOMRect → Dimensions
domRectToDimensions domRect =
  { width: domRect.width
  , height: domRect.height
  }
