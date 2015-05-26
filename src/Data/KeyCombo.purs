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

import Data.Array (nub, sort, snoc)
import Data.Char (Char(), fromCharCode)
import Data.Foldable (foldl)
import Data.String (charAt, fromChar, joinWith, toUpper)

import qualified Data.String.Unsafe as U

data KeyCombo
  = Meta
  | Alt
  | Shift
  | Letter Char
  | Enter
  | Combo [KeyCombo]

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

combo :: [KeyCombo] -> KeyCombo
combo ks = Combo $ sort $ nub $ foldl snoc [] ks

instance eqKeyCombo :: Eq KeyCombo where
  (==) Meta Meta = true
  (==) Alt Alt = true
  (==) Shift Shift = true
  (==) (Letter x) (Letter y) = x == y
  (==) Enter Enter = true
  (==) (Combo xs) (Combo ys) = xs == ys
  (==) _ _ = false
  (/=) x y = not (x == y)

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
  (<>) (Combo xs) (Combo ys) = Combo $ sort $ nub $ xs ++ ys
  (<>) (Combo xs) y = Combo $ sort $ nub $ xs ++ [y]
  (<>) x (Combo ys) = Combo $ sort $ nub $ [x] ++ ys
  (<>) x y | x == y = x
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
