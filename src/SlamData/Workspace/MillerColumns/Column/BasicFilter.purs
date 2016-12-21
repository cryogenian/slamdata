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

module SlamData.Workspace.MillerColumns.Column.BasicFilter where

import SlamData.Prelude

import Data.Minimatch as MM
import Data.String as Str
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF

mkFilter ∷ String → String → Boolean
mkFilter filter =
  let
    glob
      | filter == "" = Nothing
      | isGlob filter = Just filter
      | otherwise = Just ("*" <> filter <> "*")
  in maybe (const true) (\g → MM.minimatch (Str.toLower g) ∘ Str.toLower) glob

isGlob ∷ String → Boolean
isGlob = RX.test $ unsafePartial $ fromRight $ RX.regex "[*!?+@]" RXF.global
