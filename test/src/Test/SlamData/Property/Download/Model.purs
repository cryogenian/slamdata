module Test.SlamData.Property.Download.Model where

import Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (Either(..))

import Test.StrongCheck (QC(), Result(..), Arbitrary, arbitrary, quickCheck, (<?>))
import SlamData.Download.Model as D

newtype ArbArrayMode = ArbArrayMode D.ArrayMode

runArbArrayMode :: ArbArrayMode -> D.ArrayMode
runArbArrayMode (ArbArrayMode m) = m

instance arbitraryArbArrayMode :: Arbitrary ArbArrayMode where
  arbitrary = do
    isFlatten <- arbitrary
    map ArbArrayMode $ if isFlatten
                       then pure D.Flatten
                       else map D.Separate arbitrary

checkArrayMode :: QC Unit
checkArrayMode = quickCheck $ runArbArrayMode >>> \am ->
  case decodeJson (encodeJson am) of
    Left err -> Failed $ "Decode failed: " <> err
    Right am' -> am == am' <?> "Decoded array mode doesn't match ecoded array mode"


newtype ArbMultiValueMode = ArbMultiValueMode D.MultiValueMode

runArbMultiValueMode :: ArbMultiValueMode -> D.MultiValueMode
runArbMultiValueMode (ArbMultiValueMode mm) = mm

instance arbitraryArbMultiValueMode :: Arbitrary ArbMultiValueMode where
  arbitrary = do
    isArrayWrapped <- arbitrary
    pure $ ArbMultiValueMode $ if isArrayWrapped
                               then D.ArrayWrapped
                               else D.LineDelimited

checkMultiValueMode :: QC Unit
checkMultiValueMode = quickCheck $ runArbMultiValueMode >>> \mm ->
  case decodeJson (encodeJson mm) of
    Left err -> Failed $ "Decode failed: " <> err
    Right mm' -> mm == mm' <?> "Decoded multi value mode doesn't match encoded"

newtype ArbPrecisionMode = ArbPrecisionMode D.PrecisionMode

runArbPrecisionMode :: ArbPrecisionMode -> D.PrecisionMode
runArbPrecisionMode (ArbPrecisionMode pm) = pm

instance arbitraryArbPrecisionMode :: Arbitrary ArbPrecisionMode where
  arbitrary = do
    isReadable <- arbitrary
    pure $ ArbPrecisionMode $ if isReadable
                              then D.Readable
                              else D.Precise

checkPrecisionMode :: QC Unit
checkPrecisionMode = quickCheck $ runArbPrecisionMode >>> \pm ->
  case decodeJson (encodeJson pm) of
    Left err -> Failed $ "Decode failed: " <> err
    Right pm' -> pm == pm' <?> "Decoded precision mode doesn't match encoded"

newtype ArbCSVOptions = ArbCSVOptions D.CSVOptions

instance arbitraryArbCSVOptions :: Arbitrary ArbCSVOptions where
  arbitrary = do
    r <- { colDelimiter: _
         , rowDelimiter: _
         , quoteChar: _
         , escapeChar: _
         , arrays: _
         }
         <$> arbitrary
         <*> arbitrary
         <*> arbitrary
         <*> arbitrary
         <*> (map runArbArrayMode arbitrary)
    pure $ ArbCSVOptions $ D.CSVOptions r

runArbCSVOptions :: ArbCSVOptions -> D.CSVOptions
runArbCSVOptions (ArbCSVOptions o) = o

newtype ArbJSONOptions = ArbJSONOptions D.JSONOptions

runArbJSONOptions :: ArbJSONOptions -> D.JSONOptions
runArbJSONOptions (ArbJSONOptions o) = o

instance arbitraryArbJSONOptions :: Arbitrary ArbJSONOptions where
  arbitrary = do
    r <- { multivalues: _, precision: _ }
         <$> (map runArbMultiValueMode arbitrary)
         <*> (map runArbPrecisionMode arbitrary)
    pure $ ArbJSONOptions $ D.JSONOptions r

checkCSVOptions :: QC Unit
checkCSVOptions = quickCheck $ runArbCSVOptions >>> \opts ->
  case decodeJson (encodeJson opts) of
    Left err -> Failed $ "Decode failed: " <> err
    Right opts' -> opts == opts' <?> "Decoded csv options doesn't match encoded"

checkJSONOptions :: QC Unit
checkJSONOptions = quickCheck $ runArbJSONOptions >>> \opts ->
  case decodeJson (encodeJson opts) of
    Left err -> Failed $ "Decode failed: " <> err
    Right opts' -> opts == opts' <?> "Decoded json options doesn't match encoded"

check :: QC Unit
check = do
  checkPrecisionMode
  checkMultiValueMode
  checkArrayMode
  checkCSVOptions
  checkJSONOptions
