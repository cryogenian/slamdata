module Model.Action where

import Data.Either

data Action = View | Edit

string2action :: String -> Either String Action
string2action "view" = Right View
string2action "edit" = Right Edit
string2action _ = Left "incorrect action string"

isView :: Action -> Boolean
isView View = true
isView _ = false

isEdit :: Action -> Boolean
isEdit = not <<< isView 

instance resumeEq :: Eq Action where
  (==) View View = true
  (==) Edit Edit = true
  (==) _ _ = false
  (/=) a b = not $ a == b
