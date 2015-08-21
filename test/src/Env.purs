module Test.Env
  ( ENV()
  , getEnv
  )
  where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())

foreign import data ENV :: !
foreign import getEnv :: forall eff. String -> Eff (env :: ENV, err :: EXCEPTION | eff) String

