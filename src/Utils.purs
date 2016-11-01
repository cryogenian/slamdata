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

module Utils where

import SlamData.Prelude

import Data.Argonaut as J

import Global (readFloat, isNaN)

stringToNumber ∷ String → Maybe Number
stringToNumber s =
  let n = readFloat s in
  if isNaN n
  then Nothing
  else Just n

singletonValue' ∷ ∀ a m. (Foldable m) ⇒ m a → Either Int (Maybe a)
singletonValue' =
  foldr f initial
  where
  f x (Right (Just _)) = Left 1
  f x (Right Nothing) = Right $ Just x
  f x (Left i) = Left $ i + 1
  initial = Right Nothing

singletonValue ∷ ∀ a m n. (Applicative m, Foldable n) ⇒ m a → (Int → m a) → n a → m a
singletonValue noElements tooManyElements =
  either tooManyElements (maybe noElements pure) <<< singletonValue'

passover ∷ ∀ a b m. (Applicative m) ⇒ (a → m b) → a → m a
passover f x =
  f x *> pure x


foreign import prettyJson ∷ J.Json → String
