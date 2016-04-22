module Test.Property.Data.SQL2.Literal
  ( ArbLiteralF
  , runArbLiteralF
  , check
  ) where

import Prelude
import Data.Either as E
import Data.Eq1 (eq1)
import Data.Functor ((<$))
import Data.SQL2.Literal (LiteralF, renderLiteralF, parseLiteralF, arbitraryLiteralF)
import Test.StrongCheck ((<?>))
import Test.StrongCheck (class Arbitrary, QC, Result(Failed), quickCheck, arbitrary) as SC
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as PS

-- | We will generate and test only a single layer `LiteralF a` rather than
-- | arbitrarily deep values of type `Literal`. Because parsing and printing of
-- | `Literal` is compositional (i.e. they are defined each initially for only a
-- | the outer layer, and then extended over the full recursive type separately),
-- | we may use the induction principle of `Literal` to extend the correctness
-- | guarantees of our tests to the full recursive type.
newtype ArbLiteralF a = ArbLiteralF (LiteralF a)

runArbLiteralF
  :: forall a
   . ArbLiteralF a
  -> LiteralF a
runArbLiteralF (ArbLiteralF c) =
  c

instance arbitraryArbLiteralF :: (SC.Arbitrary a) => SC.Arbitrary (ArbLiteralF a) where
  arbitrary = ArbLiteralF <$> arbitraryLiteralF SC.arbitrary

data InductiveHypothesis = InductiveHypothesis
type ArbLiteral = ArbLiteralF InductiveHypothesis

instance arbitraryInductiveHypothesis :: SC.Arbitrary InductiveHypothesis where
  arbitrary = pure InductiveHypothesis

instance eqInductiveHypothesis :: Eq InductiveHypothesis where
  eq _ _ = true

instance showInductiveHypothesis :: Show InductiveHypothesis where
  show InductiveHypothesis = "InductiveHypothesis"

parseInductiveHypothesis
  :: forall m
   . (Monad m)
  => P.ParserT String m InductiveHypothesis
parseInductiveHypothesis =
  InductiveHypothesis <$
    PS.string "InductiveHypothesis"

check :: SC.QC Unit
check =
  SC.quickCheck $ runArbLiteralF >>> \c ->
    case P.runParser (renderLiteralF show c) (parseLiteralF parseInductiveHypothesis) of
      E.Left err ->
        SC.Failed $
          "Parse failed: "
            <> show err
            <> " when parsing "
            <> show c
      E.Right d ->
        eq1 c d <?>
          "Literal mismatch: In came "
             <> renderLiteralF show c
             <> " but out went "
             <> renderLiteralF show d

