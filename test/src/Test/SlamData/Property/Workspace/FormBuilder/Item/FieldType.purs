module Test.SlamData.Property.Workspace.FormBuilder.Item.FieldType
  ( ArbFieldType
  , runArbFieldType
  ) where

import Prelude
import SlamData.Workspace.FormBuilder.Item.FieldType (FieldType, allFieldTypes)
import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen (allInArray)

newtype ArbFieldType = ArbFieldType FieldType

runArbFieldType
  :: ArbFieldType
  -> FieldType
runArbFieldType (ArbFieldType ft) =
  ft

instance arbitraryArbFieldType :: Arbitrary ArbFieldType where
  arbitrary =
    ArbFieldType <$>
      allInArray allFieldTypes
