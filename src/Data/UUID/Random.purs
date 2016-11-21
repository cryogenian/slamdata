{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Data.UUID.Random
  ( UUIDv4
  , make
  , toString
  , fromString
  ) where

import Prelude
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random as Random
import Control.MonadZero (guard)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Traversable (sequence, traverse)
import Data.Maybe (Maybe(..), fromJust)
import Data.String as String
import Partial.Unsafe (unsafePartial)
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data UUIDv4 = UUIDv4 String (Array Int)

make ∷ ∀ m eff. MonadEff (random ∷ Random.RANDOM | eff) m ⇒ m UUIDv4
make = liftEff $ make' Random.randomInt

make' ∷ ∀ f. Applicative f ⇒ (Int → Int → f Int) → f UUIDv4
make' rand = unsafeToUUIDv4 <$> sequence uuid
  where
    uuid = x8 <> x4 <> [ pure 4 ] <> x3 <> y <> x3 <> x12
    y = [ rand 8 11 ]
    x = [ rand 0 15 ]
    x3 = x <> x <> x
    x4 = x3 <> x
    x8 = x4 <> x4
    x12 = x8 <> x4

toString ∷ UUIDv4 → String
toString (UUIDv4 s _) = s

toString' ∷ Array Int → String
toString' is =
  String.joinWith "-"
    [ print 0 8
    , print 8 12
    , print 12 16
    , print 16 20
    , print 20 32
    ]
  where
    print i j =
      Array.slice i j is
        # map (Int.toStringAs hexRadix)
        # String.joinWith ""

fromString ∷ String → Maybe UUIDv4
fromString str = do
  let
    ds = String.split (String.Pattern "") =<< String.split (String.Pattern "-") str
  is ← traverse (Int.fromStringAs hexRadix) ds
  rd ← Array.index is 12
  yd ← Array.index is 16
  guard $ Array.length is == 32
  guard $ rd == 4
  guard $ yd >= 8 && yd < 12
  pure (unsafeToUUIDv4 is)

unsafeToUUIDv4 ∷ Array Int → UUIDv4
unsafeToUUIDv4 is = UUIDv4 (toString' is) is

hexRadix ∷ Int.Radix
hexRadix = unsafePartial (fromJust (Int.radix 16))

instance eqUUIDv4 ∷ Eq UUIDv4 where
  eq (UUIDv4 s1 _) (UUIDv4 s2 _) = s1 == s2

instance ordUUIDv4 ∷ Ord UUIDv4 where
  compare (UUIDv4 s1 _) (UUIDv4 s2 _) = compare s1 s2

instance showUUIDv4 ∷ Show UUIDv4 where
  show (UUIDv4 s1 _) = "UUIDv4 " <> show s1

instance arbitraryUUIDv4 ∷ SC.Arbitrary UUIDv4 where
  arbitrary =
    make' Gen.chooseInt

instance encodeJson ∷ EncodeJson UUIDv4 where
  encodeJson (UUIDv4 s _) = encodeJson s

instance decodeJson ∷ DecodeJson UUIDv4 where
  decodeJson json = do
    s ← decodeJson json
    case fromString s of
      Just u  → Right u
      Nothing → Left ("Invalid UUIDv4: " <> s)
