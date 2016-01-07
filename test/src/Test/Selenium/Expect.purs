module Test.Selenium.Expect where

import Prelude

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (MonadError, throwError)
import Selenium.Monad (Selenium())
import Data.String (contains)
import Data.String.Regex (Regex(), test)
import Global (readInt)

type Expectation a b = { f :: a -> b -> Boolean, s :: String }

expect :: forall a b o e. (Show a, Show b) => a -> Expectation a b -> b -> Selenium o e Unit
expect expected expectation actual | expectation.f expected actual = return unit
expect expected expectation actual = throwError $ error $ msg
  where
  msg = "Expected " ++ show actual ++ " to " ++ expectation.s ++ " " ++ show expected ++ "."

toEq :: forall a. (Eq a) => Expectation a a
toEq = { f: (==), s: "equal" }

toNotEq :: forall a. (Eq a) => Expectation a a
toNotEq = { f: (/=), s: "not equal" }

toBeGreaterThan :: Expectation String Number
toBeGreaterThan = { f: (\s n -> (readInt 10 s) > n), s : "be greater than" }

toBeLessThan :: Expectation String Number
toBeLessThan = { f: (\s n -> (readInt 10 s) > n), s: "be less than" }

toMatch :: Expectation String Regex
toMatch = { f: flip test, s: "match regular expression" }

toContain :: Expectation String String
toContain = { f: contains, s: "contain" }

