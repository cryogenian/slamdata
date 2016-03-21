module Test.SlamData.Property.Notebook.FormBuilder.Model
  ( ArbModel
  , runArbModel
  , check
  ) where

import Prelude
import Data.Either as E
import SlamData.Notebook.FormBuilder.Model as FB
import SlamData.Notebook.FormBuilder.Item.Model (EqModel(..))
import Test.SlamData.Property.Notebook.FormBuilder.Item.Model as Item
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
        (EqModel <$> model.items) == (EqModel <$> model'.items) <?> "items mismatch"
