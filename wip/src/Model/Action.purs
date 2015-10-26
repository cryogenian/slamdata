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

module Model.Action where

import Prelude
import Data.Either (Either(..))

data Action = View | Edit

string2action :: String -> Either String Action
string2action "view" = Right View
string2action "edit" = Right Edit
string2action _ = Left "incorrect action string"

printAction :: Action -> String
printAction View = "view"
printAction Edit = "edit"

isView :: Action -> Boolean
isView View = true
isView _ = false

isEdit :: Action -> Boolean
isEdit = not <<< isView

instance resumeEq :: Eq Action where
  eq View View = true
  eq Edit Edit = true
  eq _ _ = false
