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

module SlamData.Workspace.Action
  ( Action(..)
  , parseAction
  , printAction
  , toAccessType
  , module SlamData.Workspace.AccessType
  ) where

import SlamData.Prelude

import Data.Path.Pathy as Pt
import Data.String as Str

import SlamData.Workspace.AccessType (AccessType(..))

import Utils.Path as UP

data Action
  = New
  | Load AccessType
  | Exploring UP.FilePath

-- | Used in route parsing
parseAction :: String -> Either String Action
parseAction "view" = Right (Load ReadOnly)
parseAction "edit" = Right (Load Editable)
parseAction "new" = Right New
-- This is useless, added for consistensy
parseAction str =
  Str.stripPrefix (Str.Pattern "exploring") str
    >>= UP.parseFilePath
    # maybe (Left "incorrect action string") (Right ∘ Exploring)

-- | Used in route construction
printAction :: Action -> String
printAction (Load Editable) = "edit"
printAction (Load ReadOnly) = "view"
printAction New = "new"
printAction (Exploring fp) = "exploring" ⊕ UP.encodeURIPath (Pt.printPath fp)

toAccessType :: Action -> AccessType
toAccessType New = Editable
toAccessType (Load t) = t
toAccessType (Exploring _) = Editable

derive instance eqAccessType ∷ Eq Action

derive instance ordAccessType ∷ Ord Action
