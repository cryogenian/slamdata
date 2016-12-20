module Test.SlamData.Property.Workspace.Card.Ace.Model
  ( ArbModel
  , runArbModel
  , check
  ) where

import Prelude

import Data.Either (Either(..))
import SlamData.Workspace.Card.Ace.Model as M
import Utils.Ace (RangeRec)

import Test.StrongCheck (SC, Result(..), quickCheck, (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)

newtype ArbRangeRec = ArbRangeRec RangeRec

runArbRangeRec ∷ ArbRangeRec → RangeRec
runArbRangeRec (ArbRangeRec r) = r

instance arbitraryArbRangeRec ∷ Arbitrary ArbRangeRec where
  arbitrary = do
    r ← { startColumn: _, startRow: _, endColumn: _, endRow: _ }
         <$> arbitrary
         <*> arbitrary
         <*> arbitrary
         <*> arbitrary
    pure $ ArbRangeRec r

newtype ArbModel = ArbModel M.Model

runArbModel ∷ ArbModel → M.Model
runArbModel (ArbModel m) = m

instance arbitraryArbModel ∷ Arbitrary ArbModel where
  arbitrary = do
    r ← { text: _, ranges: _ }
        <$> arbitrary
        <*> (map (map runArbRangeRec) arbitrary)
    pure $ ArbModel r

check ∷ forall eff. SC eff Unit
check = quickCheck $ runArbModel >>> \m →
  case M.decode (M.encode m) of
    Left err → Failed $ "Decode failed: " <> err
    Right m' → M.eqModel m m' <?> "Decoced ace card model doesn't match encoded"
