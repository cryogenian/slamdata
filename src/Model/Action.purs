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

