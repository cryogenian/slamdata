module Test.Property.Notebook.FormBuilder.Item.FieldType
  ( ArbFieldType()
  , runArbFieldType
  ) where

import Prelude
import Notebook.FormBuilder.Item.FieldType
import Test.StrongCheck as SC
import Test.StrongCheck.Gen as SC

newtype ArbFieldType = ArbFieldType FieldType

runArbFieldType
  :: ArbFieldType
  -> FieldType
runArbFieldType (ArbFieldType ft) =
  ft

instance arbitraryArbFieldType :: SC.Arbitrary ArbFieldType where
  arbitrary =
    ArbFieldType <$>
      SC.allInArray allFieldTypes

