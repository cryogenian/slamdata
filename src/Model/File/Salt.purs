module Model.File.Salt where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (Random(), randomInt)

newtype Salt = Salt String

runSalt :: Salt -> String
runSalt (Salt s) = s

instance eqSalt :: Eq Salt where
  (==) (Salt x) (Salt y) = x == y
  (/=) x y = not (x == y)

instance showSalt :: Show Salt where
  show (Salt s) = "Salt " ++ show s

newSalt :: forall e. Eff (random :: Random | e) Salt
newSalt = Salt <<< show <$> randomInt 1000000 2000000
