{-
Copyright 2015 SlamData, Inc.

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

module Model.Notebook.Action
  ( Action(..)
  , parseAction
  , printAction
  , toAccessType
  , module Model.AccessType
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Generic (Generic, gEq, gCompare)

import Model.AccessType (AccessType(..))

data Action = New | Load AccessType

-- | Used in route parsing
parseAction :: String -> Either String Action
parseAction "view" = Right (Load ReadOnly)
parseAction "edit" = Right (Load Editable)
parseAction "new" = Right New
parseAction _ = Left "incorrect action string"

-- | Used in route construction
printAction :: Action -> String
printAction (Load Editable) = "edit"
printAction (Load ReadOnly) = "view"
printAction New = "new"

toAccessType :: Action -> AccessType
toAccessType New = Editable
toAccessType (Load t) = t

derive instance genericAccessType :: Generic Action
instance eqAccessType :: Eq Action where eq = gEq
instance ordAccessType :: Ord Action where compare = gCompare
