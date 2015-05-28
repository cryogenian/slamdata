module Model.Salt where

newtype Salt = Salt String

runSalt :: Salt -> String
runSalt (Salt s) = s

instance eqSalt :: Eq Salt where
  (==) (Salt x) (Salt y) = x == y
  (/=) x y = not (x == y)

instance showSalt :: Show Salt where
  show (Salt s) = "Salt " ++ show s
