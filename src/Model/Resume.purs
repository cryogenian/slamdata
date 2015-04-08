module Model.Resume where

import Data.Either

data Resume = View | Edit

string2resume :: String -> Either String Resume
string2resume "view" = Right View
string2resume "edit" = Right Edit
string2resume _ = Left "incorrect resume string"

instance resumeEq :: Eq Resume where
  (==) View View = true
  (==) Edit Edit = true
  (==) _ _ = false
  (/=) a b = not $ a == b
