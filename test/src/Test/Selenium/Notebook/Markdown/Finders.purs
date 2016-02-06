module Test.Selenium.Notebook.Markdown.Finders where

import Prelude

import Test.Selenium.Monad (Check(), byAriaLabel)
import Test.Selenium.Finders (findByXPath, findAllByXPath, findAnyByXPath)
import Data.List (List(), length)
import Selenium.Monad (byXPath, tryRepeatedlyTo, getText, findElements)
import Selenium.Combinators (tryToFind)
import Selenium.Types (Element())
import Test.XPath as XPath
import Test.Selenium.XPaths as XPaths

import Data.Traversable (traverse) as T

findMdField :: Check Element
findMdField = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.aceEditorXPath

findMdPlayButton :: Check Element
findMdPlayButton = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.playXPath

findMdQueryPlayButton :: Check Element
findMdQueryPlayButton = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.playXPath

findCreateMdQueryCellButton :: Check Element
findCreateMdQueryCellButton =
  findByXPath $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Insert Query cell after this cell"

findMdQueryField :: Check Element
findMdQueryField = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.aceEditorXPath

--findMdQueryColumnCellsByHeading :: String -> Check (List Element)
--findMdQueryColumnCellsByHeading = findFollowingTableColumnCellsByHeading XPaths.mdQueryCellTitleXPath
--
--findMdQueryColumnCellsTextByHeading :: String -> Check (List String)
--findMdQueryColumnCellsTextByHeading s = findMdQueryColumnCellsByHeading s >>= T.traverse getText

