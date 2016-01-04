module Test.Property.Notebook.FormBuilder.Model
  ( ArbModel()
  , runArbModel
  , check
  ) where

import Prelude
import Data.Either as E
import Notebook.FormBuilder.Model as FB
import Notebook.FormBuilder.Item.Model as Item
import Test.Property.Notebook.FormBuilder.Item.Model as Item
import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC

newtype ArbModel = ArbModel FB.Model

runArbModel
  :: ArbModel
  -> FB.Model
runArbModel (ArbModel m) =
  m

instance arbitraryArbModel :: SC.Arbitrary ArbModel where
  arbitrary =
    ArbModel <<< { items : _ } <<< map Item.runArbModel
      <$> SC.arbitrary

check :: SC.QC Unit
check =
  SC.quickCheck $ runArbModel >>> \model ->
    case FB.decode (FB.encode model) of
      E.Left err -> SC.Failed $ "Decode failed: " ++ err
      E.Right model' ->
        (Item.EqModel <$> model.items) == (Item.EqModel <$> model'.items) <?> "items mismatch"
