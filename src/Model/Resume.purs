module Model.Action where

import Data.Either

data Action = View | Edit

string2action :: String -> Either String Action
string2action "view" = Right View
string2action "edit" = Right Edit
string2action _ = Left "incorrect action string"

instance resumeEq :: Eq Action where
  (==) View View = true
  (==) Edit Edit = true
  (==) _ _ = false
  (/=) a b = not $ a == b
