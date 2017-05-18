module Test.SlamData.Property.Workspace.FormBuilder.Model
  ( ArbModel
  , runArbModel
  , check
  ) where

import Prelude
import Data.Either as E
import SlamData.Workspace.FormBuilder.Model as FB
import SlamData.Workspace.FormBuilder.Item.Model (EqModel(..))
import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC
import Test.StrongCheck.Arbitrary as SCA

newtype ArbModel = ArbModel FB.Model

runArbModel
  :: ArbModel
  -> FB.Model
runArbModel (ArbModel m) =
  m

instance arbitraryArbModel :: SCA.Arbitrary ArbModel where
  arbitrary = ArbModel <$> FB.genModel

check :: forall eff. SC.SC eff Unit
check =
  SC.quickCheck $ runArbModel >>> \model ->
    case FB.decode (FB.encode model) of
      E.Left err -> SC.Failed $ "Decode failed: " <> err
      E.Right model' ->
        (EqModel <$> model.items) == (EqModel <$> model'.items) <?> "items mismatch"
