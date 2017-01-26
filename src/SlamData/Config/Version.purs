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

module SlamData.Config.Version where

import SlamData.Prelude

import Data.Array as A
import Data.String.Regex as Rgx
import Data.String.Regex.Unsafe as Rgu
import Data.String.Regex.Flags as RXF

foreign import slamDataVersion ∷ String

shortVersion ∷ Maybe String
shortVersion = do
  matches ← Rgx.match versionRegex slamDataVersion
  major ← join $ matches A.!! 1
  minor ← join $ matches A.!! 2
  pure $ major ⊕ "." ⊕ minor
  where
  versionRegex ∷ Rgx.Regex
  versionRegex =
    Rgu.unsafeRegex "^\\D*(\\d+)\\.(\\d+)" RXF.noFlags
