module Test.Selenium.Notebook (test) where

import Prelude
import Test.Selenium.Monad (Check(), stop)
import Test.Selenium.Notebook.Contexts (setUp)
import qualified Test.Selenium.Notebook.Explore as Explore
import qualified Test.Selenium.Notebook.Search as Search 

test :: Check Unit
test = do
  setUp
  Explore.test
--  Search.test
