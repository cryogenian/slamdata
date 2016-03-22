module Test.Property.Data.SQL2.Literal
  ( ArbLiteralF
  , runArbLiteralF
  , check
  ) where

import Prelude
import Data.Either as E
import Data.Eq1 (eq1)
import Data.Functor ((<$))
import Data.Int as Int
import Data.List as L
import Data.SQL2.Literal (LiteralF(..), renderLiteralF, parseLiteralF)
import Data.StrMap as SM
import Data.Tuple as T
import Test.StrongCheck ((<?>))
import Test.StrongCheck (class Arbitrary, QC, Result(Failed), quickCheck, arbitrary) as SC
import Test.StrongCheck.Gen (Gen, oneOf, arrayOf) as SC
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

arbitraryStrMap
  :: forall a
   . SC.Gen a
  -> SC.Gen (SM.StrMap a)
arbitraryStrMap arb =
  map (SM.fromList <<< L.toList) <<< SC.arrayOf $
    T.Tuple <$> SC.arbitrary <*> arb

arbitraryDecimal :: SC.Gen Number
arbitraryDecimal = Int.toNumber <$> SC.arbitrary

instance arbitraryArbLiteralF :: (SC.Arbitrary a) => SC.Arbitrary (ArbLiteralF a) where
  arbitrary = do
    SC.oneOf (pure $ ArbLiteralF Null) $
      map ArbLiteralF <$>
        [ Boolean <$> SC.arbitrary
        , Integer <$> SC.arbitrary
        , Decimal <$> arbitraryDecimal
        , String <$> SC.arbitrary
        , DateTime <$> SC.arbitrary
        , Date <$> SC.arbitrary
        , Time <$> SC.arbitrary
        , Interval <$> SC.arbitrary
        , ObjectId <$> SC.arbitrary
        , OrderedSet <$> SC.arbitrary
        , Array <$> SC.arbitrary
        , Object <$> arbitraryStrMap SC.arbitrary
        ]

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

