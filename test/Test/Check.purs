module Test.Check where

import Test.StrongCheck


data Dummy = Dummy

instance eqDummy :: Eq Dummy where
  (==) a b = true
  (/=) a b = false

instance dummyStrongCheck :: Arbitrary Dummy where
  arbitrary = return Dummy

checkDummy :: Dummy -> Boolean
checkDummy d = d == Dummy

check = quickCheck checkDummy
