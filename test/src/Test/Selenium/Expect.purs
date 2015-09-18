module Test.Selenium.Expect where

import Prelude

import Control.Monad.Eff.Exception (error, Error())
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Selenium.Monad (Selenium())
import Data.String (contains)
import Data.String.Regex (Regex(), test)

expect :: forall a b o e. (Show a, Show b) => (a -> b -> Boolean) -> String -> a -> b -> Selenium o e Unit
expect expectation description expected actual | expectation expected actual = return unit
expect expectation description expected actual = throwError $ error $ msg
  where
  msg = "Expected " ++ show actual ++ " to " ++ description ++ " " ++ show expected ++ "."

expectEq :: forall a o e. (Eq a, Show a) => a -> a -> Selenium o e Unit
expectEq = expect (==) "equal"

expectNotEq :: forall a o e. (Eq a, Show a) => a -> a -> Selenium o e Unit
expectNotEq = expect (/=) "not equal"

expectMatch :: forall a o e. Regex -> String -> Selenium o e Unit
expectMatch = expect test "match regular expression"

expectContains :: forall a o e. String -> String -> Selenium o e Unit
expectContains = expect contains "contain"
