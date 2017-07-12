module Test.SlamData.Property.Download.Model where

import SlamData.Prelude

import Control.Monad.Gen as Gen
import Data.Argonaut as J
import Data.Argonaut.Encode ((:=), (~>))
import Data.Codec.Argonaut as CA
import Data.String.Gen (genAsciiString)
import SlamData.Download.Model as D
import Test.StrongCheck (Result(..), SC, quickCheck, (===))
import Test.StrongCheck.Gen (Gen)

-- Do not touch the JSON generation here unless we're making a breaking change
-- to the format!
genArrayModeJson ∷ ∀ m. MonadRec m ⇒ MonadGen m ⇒ m (D.ArrayMode × J.Json)
genArrayModeJson = Gen.choose genFlatten genSeparate
  where
  genFlatten =
    pure
      $ D.Flatten
      × "ty" := "flatten"
      ~> "val" := ""
      ~> J.jsonEmptyObject
  genSeparate = do
    value ← genAsciiString
    pure
      $ D.Separate value
      × "ty" := "separate"
      ~> "val" := value
      ~> J.jsonEmptyObject

propArrayModeDecode ∷ Gen Result
propArrayModeDecode = do
  (expected × json) ← genArrayModeJson
  pure case CA.decode D.codecArrayMode json of
    Left err -> Failed $ "Decode failed:\n" <> CA.printJsonDecodeError err
    Right am -> am === expected

check :: forall eff. SC eff Unit
check = do
  quickCheck propArrayModeDecode
