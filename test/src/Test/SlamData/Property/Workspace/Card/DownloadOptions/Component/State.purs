module Test.SlamData.Property.Workspace.Card.DownloadOptions.Component.State
  ( ArbState
  , runArbState
  , check
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import SlamData.Workspace.Card.DownloadOptions.Component.State as M
import SlamData.Workspace.LevelOfDetails as LOD

import Test.Property.Utils.Path (runArbFilePath)
import Test.SlamData.Property.Download.Model (runArbCSVOptions, runArbJSONOptions)
import Test.StrongCheck (QC, Result(..), class Arbitrary, arbitrary, quickCheck, (<?>))

newtype ArbState = ArbState M.State

runArbState :: ArbState -> M.State
runArbState (ArbState s) = s

instance arbitraryArbState :: Arbitrary ArbState where
  arbitrary = do
    r <- { compress: _, options: _, source: _, levelOfDetails: LOD.High}
         <$> arbitrary
         <*> (map (bimap runArbCSVOptions runArbJSONOptions) arbitrary)
         <*> (map (map runArbFilePath) arbitrary)
    pure $ ArbState r

check :: QC Unit
check = quickCheck $ runArbState >>> \m ->
  case M.decode (M.encode m) of
    Left err -> Failed $ "Decode failed: " <> err
    Right m' -> (   m.compress == m'.compress
                 && m.options == m'.options
                 && m.source == m'.source)
                <?> "Decoded download card model doesn't match encoded"
