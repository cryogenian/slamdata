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
import Data.Lens (LensP, lens)
import Data.NonEmpty (NonEmpty(..))
import Data.Path.Pathy (parsePath, rootDir, (</>))
import Data.Profunctor.Strong (first, second)
import Data.String.Regex as Rx
import Data.URI (Host, Port) as URI
import Data.URI.Host (parseHost) as URI

import Text.Parsing.StringParser (runParser)

import Utils.Path as PU

type MountHost = String × String

type MountProp = String × String

type Host = URI.Host × (Maybe URI.Port)

_hosts ∷ ∀ r a. LensP { hosts ∷ a | r } a
_hosts = lens _.hosts (_ { hosts = _ })

_path ∷ ∀ r a. LensP { path ∷ a | r } a
_path = lens _.path (_ { path = _ })

_user ∷ ∀ r a. LensP { user ∷ a | r } a
_user = lens _.user (_{user = _})

_password ∷ ∀ r a. LensP { password ∷ a | r } a
_password = lens _.password (_{password = _})

_props ∷ ∀ r a. LensP { props ∷ a | r } a
_props = lens _.props (_ { props = _ })

_host' ∷ ∀ r a. LensP { host ∷ a | r } a
_host' = lens _.host (_ { host = _ })

_host ∷ LensP MountHost String
_host = first

_port ∷ LensP MountHost String
_port = second

initialTuple ∷ Tuple String String
initialTuple = Tuple "" ""

isEmptyTuple ∷ Tuple String String → Boolean
isEmptyTuple (Tuple k v) = isEmpty k && isEmpty v

isEmpty ∷ String → Boolean
isEmpty = Rx.test rxEmpty

rxEmpty ∷ Rx.Regex
rxEmpty = unsafePartial fromRight $ Rx.regex "^\\s*$" Rx.noFlags

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
  host' ← lmap show $ runParser URI.parseHost host
  port' ← case nonEmptyString port of
    Nothing → pure Nothing
    Just p →
      maybe
        (Left $ "'" ⊕ port ⊕ "' is not a valid port number")
        (Right <<< Just) $
        (Int.fromString p)
  pure $ Tuple host' port'

processDynMap ∷ Array (String × String) → Array (String × String)
processDynMap as =
  A.filter (not isEmptyTuple) as ⊕ [initialTuple]
