module Test.Property.Notebook.FormBuilder.Item.Model
  ( ArbModel()
  , runArbModel
  , check
  ) where

import Prelude
import Data.Either as E
import Data.Foldable as F
import Notebook.FormBuilder.Item.Model as Item
import Test.Property.Notebook.FormBuilder.Item.FieldType as FT
import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC

newtype ArbModel = ArbModel Item.Model

runArbModel
  :: ArbModel
  -> Item.Model
runArbModel (ArbModel m) =
  m

instance arbitraryArbModel :: SC.Arbitrary ArbModel where
  arbitrary = do
    name <- SC.arbitrary
    fieldType <- FT.runArbFieldType <$> SC.arbitrary
    defaultValue <- SC.arbitrary
    pure $ ArbModel { name, fieldType, defaultValue }

check :: SC.QC Unit
check =
  SC.quickCheck $ runArbModel >>> \model ->
    case Item.decode (Item.encode model) of
      E.Left err -> SC.Failed $ "Decode failed: " ++ err
      E.Right model' ->
        F.mconcat
          [ model.name == model'.name <?> "name mismatch"
          , model.fieldType == model'.fieldType <?> "fieldType mismatch"
          , model.defaultValue == model'.defaultValue <?> "defaultValue mismatch"
          ]
