module Test.Property.Notebook.Cell.Download.Component.State
  ( ArbState()
  , runArbState
  , check
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Notebook.Cell.Download.Component.State as M

import Test.StrongCheck (QC(), Result(..), Arbitrary, arbitrary, quickCheck, (<?>))
import Test.Property.Model.Resource (runArbResource)
import Test.Property.Model.Download (runArbCSVOptions, runArbJSONOptions)

newtype ArbState = ArbState M.State

runArbState :: ArbState -> M.State
runArbState (ArbState s) = s

instance arbitraryArbState :: Arbitrary ArbState where
  arbitrary = do
    r <- { compress: _, options: _, source: _ }
         <$> arbitrary
         <*> (map (bimap runArbCSVOptions runArbJSONOptions) arbitrary)
         <*> (map runArbResource arbitrary)
    pure $ ArbState r

check :: QC Unit
check = quickCheck $ runArbState >>> \m ->
  case M.decode (M.encode m) of
    Left err -> Failed $ "Decode failed: " <> err
    Right m' -> (   m.compress == m'.compress
                 && m.options == m'.options
                 && m.source == m'.source)
                <?> "Decoded download cell model doesn't match encoded"
