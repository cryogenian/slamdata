module Test.Selenium.Log where

import Prelude
import Text.Chalk (red, green, magenta, yellow)
import Control.Monad.Trans (lift) 
import Control.Monad.Aff.Console (log) 
import Control.Monad.Eff.Exception (error) 
import Control.Monad.Error.Class (throwError)
import Test.Selenium.Monad (Check())

successMsg :: String -> Check Unit 
successMsg msg = void $ lift $ log $ green msg

errorMsg :: forall a. String -> Check a 
errorMsg msg = do
  lift $ log $ red msg 
  throwError $ error msg

sectionMsg :: String -> Check Unit 
sectionMsg msg = void $ lift $ log $ magenta $ "\n" <> msg

warnMsg :: String -> Check Unit
warnMsg msg = void $ lift $ log $ yellow msg
