module Test.Property.SlamData.Notebook.Cell.Ace.Model
  ( ArbModel()
  , runArbModel
  , check
  ) where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import SlamData.Notebook.Cell.Ace.Model as M
import Utils.Ace (RangeRec(), eqRangeRec)

import Test.StrongCheck (QC(), Result(..), Arbitrary, arbitrary, quickCheck, (<?>))

newtype ArbRangeRec = ArbRangeRec RangeRec

runArbRangeRec :: ArbRangeRec -> RangeRec
runArbRangeRec (ArbRangeRec r) = r

instance arbitraryArbRangeRec :: Arbitrary ArbRangeRec where
  arbitrary = do
    r <- { startColumn: _, startRow: _, endColumn: _, endRow: _ }
         <$> arbitrary
         <*> arbitrary
         <*> arbitrary
         <*> arbitrary
    pure $ ArbRangeRec r

newtype ArbModel = ArbModel M.Model

runArbModel :: ArbModel -> M.Model
runArbModel (ArbModel m) = m

instance arbitraryArbModel :: Arbitrary ArbModel where
  arbitrary = do
    r <- { text: _, ranges: _ }
         <$> arbitrary
         <*> (map (map runArbRangeRec) arbitrary)
    pure $ ArbModel r

check :: QC Unit
check = quickCheck $ runArbModel >>> \m ->
  case M.decode (M.encode m) of
    Left err -> Failed $ "Decode failed: " <> err
    Right m' -> (   m.text == m'.text
                 && foldl conj true (A.zipWith eqRangeRec m.ranges m'.ranges))
                <?> "Decoded ace cell model doesn't match encoded"
