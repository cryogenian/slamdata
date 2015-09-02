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

module Data.KeyCombo
  ( KeyCombo()
  , meta
  , alt
  , shift
  , letter
  , enter
  , combo
  , printKeyComboWin
  , printKeyComboLinux
  , printKeyComboMac
  ) where

import Prelude
import Data.Array (nub, sort, snoc)
import Data.Char (fromCharCode)
import Data.Foldable (foldl)
import Data.String (charAt, fromChar, joinWith, toUpper)

import qualified Data.String.Unsafe as U

data KeyCombo
  = Meta
  | Alt
  | Shift
  | Letter Char
  | Enter
  | Combo (Array KeyCombo)

meta :: KeyCombo
meta = Meta

alt :: KeyCombo
alt = Alt

shift :: KeyCombo
shift = Shift

letter :: Char -> KeyCombo
letter = Letter <<< U.charAt 0 <<< toUpper <<< fromChar

enter :: KeyCombo
enter = Enter

combo :: Array KeyCombo -> KeyCombo
combo ks = Combo $ sort $ nub $ foldl snoc [] ks

instance eqKeyCombo :: Eq KeyCombo where
  eq Meta Meta = true
  eq Alt Alt = true
  eq Shift Shift = true
  eq (Letter x) (Letter y) = x == y
  eq Enter Enter = true
  eq (Combo xs) (Combo ys) = xs == ys
  eq _ _ = false

instance ordKeyCombo :: Ord KeyCombo where
  compare Meta Meta = EQ
  compare Meta _ = LT
  compare _ Meta = GT
  compare Alt Alt = EQ
  compare Alt _ = EQ
  compare _ Alt = GT
  compare Shift Shift = EQ
  compare Shift _ = LT
  compare _ Shift = GT
  compare (Letter x) (Letter y) = compare x y
  compare (Letter _) _ = LT
  compare _ (Letter _) = GT
  compare Enter Enter = EQ
  compare Enter _ = LT
  compare _ Enter = GT
  compare (Combo xs) (Combo ys) = compare xs ys

instance showKeyCombo :: Show KeyCombo where
  show Meta = "Meta"
  show Alt = "Alt"
  show Shift = "Shift"
  show (Letter l) = "Letter (" ++ show l ++ ")"
  show Enter = "Enter"
  show (Combo ks) = "Combo " ++ show ks

instance semigroupKeyCombo :: Semigroup KeyCombo where
  append (Combo xs) (Combo ys) = Combo $ sort $ nub $ xs ++ ys
  append (Combo xs) y = Combo $ sort $ nub $ xs ++ [y]
  append x (Combo ys) = Combo $ sort $ nub $ [x] ++ ys
  append x y | x == y = x
             | otherwise = Combo $ sort [x, y]

printKeyComboWin :: KeyCombo -> String
printKeyComboWin Meta = "Ctrl"
printKeyComboWin Alt = "Alt"
printKeyComboWin Shift = "Shift"
printKeyComboWin (Letter c) = fromChar c
printKeyComboWin Enter = "Enter"
printKeyComboWin (Combo ks) = joinWith "+" (printKeyComboWin <$> ks)

printKeyComboLinux :: KeyCombo -> String
printKeyComboLinux = printKeyComboWin

printKeyComboMac :: KeyCombo -> String
printKeyComboMac Meta = fromChar $ fromCharCode 8984
printKeyComboMac Alt = fromChar $ fromCharCode 8997
printKeyComboMac Shift = fromChar $ fromCharCode 8679
printKeyComboMac (Letter c) = fromChar c
printKeyComboMac Enter = "Enter"
printKeyComboMac (Combo ks) = joinWith "" (printKeyComboMac <$> ks)
