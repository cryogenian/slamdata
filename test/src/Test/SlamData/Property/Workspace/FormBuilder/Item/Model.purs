module Test.SlamData.Property.Workspace.FormBuilder.Item.Model
  ( ArbModel
  , runArbModel
  , check
  ) where

import SlamData.Prelude
import SlamData.Workspace.FormBuilder.Item.Model as Item
import Test.SlamData.Property.Workspace.FormBuilder.Item.FieldType as FT
import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC
import Test.StrongCheck.Arbitrary as SCA

newtype ArbModel = ArbModel Item.Model

runArbModel ∷ ArbModel → Item.Model
runArbModel (ArbModel m) = m

instance arbitraryArbModel ∷ SCA.Arbitrary ArbModel where
  arbitrary = do
    name ← SCA.arbitrary
    fieldType ← FT.runArbFieldType <$> SCA.arbitrary
    defaultValue ← SCA.arbitrary
    pure $ ArbModel { name, fieldType, defaultValue }

check ∷ forall eff. SC.SC eff Unit
check =
  SC.quickCheck $ runArbModel >>> \model →
    case Item.decode (Item.encode model) of
      Left err → SC.Failed $ "Decode failed: " <> err
      Right model' →
        fold
          [ model.name == model'.name <?> "name mismatch"
          , model.fieldType == model'.fieldType <?> "fieldType mismatch"
          , model.defaultValue == model'.defaultValue <?> "defaultValue mismatch"
          ]
