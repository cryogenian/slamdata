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

module SlamData.FileSystem.Dialog.Mount.Common.State where

import SlamData.Prelude

import Data.Array as A
import Data.Int as Int
import Data.Lens (Lens', lens)
import Data.List.NonEmpty as NEL
import Data.NonEmpty (NonEmpty(..))
import Data.Path.Pathy (parsePath, rootDir, (</>))
import Data.Profunctor.Strong (first, second)
import Data.String.Regex as Rx
import Data.String.Regex.Flags as RXF
import Data.URI (Host, Port) as URI
import Data.URI.Host (parseHost) as URI
import Data.Validation.Semigroup as V
import Quasar.Mount.Common (Credentials(..))
import Text.Parsing.StringParser (runParser)
import Utils.Path as PU

newtype ValidationError a = ValidationError (NEL.NonEmptyList (Tuple a String))

derive instance newtypeValidationError ∷ Newtype (ValidationError a) _
derive newtype instance eqValidationError ∷ Eq a ⇒ Eq (ValidationError a)
derive newtype instance ordValidationError ∷ Ord a ⇒ Ord (ValidationError a)
derive newtype instance semigroupValidationError ∷ Semigroup (ValidationError a)

invalid ∷ forall a b. a → String → V.V (ValidationError a) b
invalid a = V.invalid <<< ValidationError <<< pure <<< Tuple a

-- TODO: One day all will be `V` and the world will be a better place, until then, there's this...
vToE ∷ ∀ a b. V.V (ValidationError a) b → Either String b
vToE = V.unV (Left <<< snd <<< NEL.head <<< unwrap) Right

type MountHost = String × String

type MountProp = String × String

type Host = URI.Host × (Maybe URI.Port)

_hosts ∷ ∀ r a. Lens' { hosts ∷ a | r } a
_hosts = lens _.hosts (_ { hosts = _ })

_path ∷ ∀ r a. Lens' { path ∷ a | r } a
_path = lens _.path (_ { path = _ })

_user ∷ ∀ r a. Lens' { user ∷ a | r } a
_user = lens _.user (_{user = _})

_password ∷ ∀ r a. Lens' { password ∷ a | r } a
_password = lens _.password (_{password = _})

_props ∷ ∀ r a. Lens' { props ∷ a | r } a
_props = lens _.props (_ { props = _ })

_host' ∷ ∀ r a. Lens' { host ∷ a | r } a
_host' = lens _.host (_ { host = _ })

_host ∷ Lens' MountHost String
_host = first

_port ∷ Lens' MountHost String
_port = second

initialTuple ∷ Tuple String String
initialTuple = Tuple "" ""

isEmptyTuple ∷ Tuple String String → Boolean
isEmptyTuple (Tuple k v) = isEmpty k && isEmpty v

isEmpty ∷ String → Boolean
isEmpty = Rx.test rxEmpty

rxEmpty ∷ Rx.Regex
rxEmpty = unsafePartial fromRight $ Rx.regex "^\\s*$" RXF.noFlags

nonEmptyString ∷ String → Maybe String
nonEmptyString s = if isEmpty s then Nothing else Just s

nonEmptyHosts ∷ Array Host → Either String (NonEmpty Array Host)
nonEmptyHosts hs = maybe err Right $ NonEmpty <$> A.head hs <*> A.tail hs
  where
  err = Left "Please enter at least one host"

parsePath' ∷ String → Maybe PU.AnyPath
parsePath' =
  bitraverse PU.sandbox PU.sandbox <<<
    parsePath (Left <<< (rootDir </> _)) Left (Right <<< (rootDir </> _)) Right

parseHost ∷ Tuple String String → Either String Host
parseHost (Tuple host port) = do
  host' ← lmap (const "Please enter a valid host") $ runParser URI.parseHost host
  port' ← case nonEmptyString port of
    Nothing → pure Nothing
    Just p →
      maybe
        (Left $ "'" ⊕ port ⊕ "' is not a valid port number")
        (Right <<< Just) $
        (Int.fromString p)
  pure $ Tuple host' port'

parseCredentials ∷ { user ∷ String, password ∷ String } → Maybe Credentials
parseCredentials { user, password } =
  case nonEmptyString user, nonEmptyString password of
    Nothing, Nothing → Nothing
    u, p → Just (Credentials { user: fromMaybe "" u, password: fromMaybe "" p })

processDynMap ∷ Array (String × String) → Array (String × String)
processDynMap as =
  A.filter (not isEmptyTuple) as ⊕ [initialTuple]
