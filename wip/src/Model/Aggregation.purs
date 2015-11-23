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

module Model.Aggregation where

import Prelude

import Control.Bind ((>=>))

import Data.Argonaut (EncodeJson, DecodeJson, decodeJson, fromString)
import Data.Bifunctor (bimap)
import Data.Generic (Generic, gEq, gCompare)
import Data.Either (Either(..))
import Data.Foldable (Foldable, foldl, sum, product)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Model.Select (OptionVal, Select(..))

data Aggregation
  = Maximum
  | Minimum
  | Average
  | Sum
  | Product

allAggregations :: Array Aggregation
allAggregations = [ Maximum
                  , Minimum
                  , Average
                  , Sum
                  , Product
                  ]
defaultAggregation :: Aggregation
defaultAggregation = Sum

printAggregation :: Aggregation -> String
printAggregation Maximum = "∧"
printAggregation Minimum = "∨"
printAggregation Average = "μ"
printAggregation Sum = "Σ"
printAggregation Product = "Π"

parseAggregation :: String -> Either String Aggregation
parseAggregation "∧" = pure Maximum
parseAggregation "∨" = pure Minimum
parseAggregation "μ" = pure Average
parseAggregation "Σ" = pure Sum
parseAggregation "Π" = pure Product
parseAggregation _ = Left "Incorrect aggregation string"

runAggregation
  :: forall a f
   . (Ord a, ModuloSemiring a, Foldable f)
  => Aggregation -> f a -> a
runAggregation Maximum nums = foldl (\b a -> if b > a then b else a) zero nums
runAggregation Minimum nums = foldl (\b a -> if b > a then a else b) zero nums
runAggregation Average nums =
  normalize
  $ foldl (\acc a -> bimap (add one) (add a) acc)  (Tuple zero zero) nums
  where
  normalize (Tuple count sum) = sum / count
runAggregation Sum nums = sum nums
runAggregation Product nums = product nums

aggregationSelect :: Select Aggregation
aggregationSelect =
  Select { value: Just Sum
         , options: allAggregations
         }



derive instance genericAggregation :: Generic Aggregation
instance eqAggregation :: Eq Aggregation where eq = gEq
instance ordAggregation :: Ord Aggregation where compare = gCompare

instance encodeJsonAggregation :: EncodeJson Aggregation where
  encodeJson = fromString <<< printAggregation
instance decodeJsonAggregation :: DecodeJson Aggregation where
  decodeJson = decodeJson >=> parseAggregation

instance optionValAggregation :: OptionVal Aggregation where
  stringVal = printAggregation
