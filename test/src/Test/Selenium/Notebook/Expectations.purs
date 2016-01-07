module Test.Selenium.Notebook.Expectations where

import Prelude
import Data.Foldable (traverse_)
import Data.String.Regex (regex, noFlags)
import Selenium.Monad (getText)
import Test.Selenium.Expect (Expectation(), expect, toMatch)
import Test.Selenium.Finders (findByXPath)
import Test.Selenium.Monad (Check())
import Test.Selenium.Notebook.Data (initialFileList)
import Test.Selenium.XPaths as XPaths
import Test.XPath as XPath

expectFinishedMessage :: String -> Check Unit
expectFinishedMessage xPath = do
  element <- findByXPath xPath
  message <- getText element
  expect message toMatch (regex "Finished: took ([0-9]*)ms." noFlags)

expectExploreFinishedMessage :: Check Unit
expectExploreFinishedMessage = expectFinishedMessage $ XPath.anywhere xPath
  where
  xPath = XPaths.exploreCellTitleXPath `XPath.following` XPaths.finishedMessageXPath

expectMdFinishedMessage :: Check Unit
expectMdFinishedMessage = expectFinishedMessage $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.finishedMessageXPath

