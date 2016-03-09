module Test.SlamData.Feature.Notebook.Markdown.Finders where

import Prelude

--import Test.SlamData.Feature.Monad ((SlamFeature(), byAriaLabel)
--import Test.SlamData.Feature.Finders (findByXPath, findAllByXPath, findAnyByXPath)
--import Data.List (List(), length)
--import Selenium.Monad (byXPath, tryRepeatedlyTo, getText, findElements)
--import Selenium.Combinators (tryToFind)
--import Selenium.Types (Element())
--import Test.XPath as XPath
--import Test.SlamData.Feature.XPaths as XPaths
--
--import Data.Traversable (traverse) as T
--
--findMdField :: SlamFeature Element
--findMdField = findByXPath $ XPath.anywhere xPath
--  where
--  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.aceEditorXPath
--
--findMdPlayButton :: SlamFeature Element
--findMdPlayButton = findByXPath $ XPath.anywhere xPath
--  where
--  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.playXPath
--
--findMdQueryPlayButton :: SlamFeature Element
--findMdQueryPlayButton = findByXPath $ XPath.anywhere xPath
--  where
--  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.playXPath
--
--findCreateMdQueryCellButton :: SlamFeature Element
--findCreateMdQueryCellButton =
--  findByXPath $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Insert Query cell after this cell"
--
--findMdQueryField :: SlamFeature Element
--findMdQueryField = findByXPath $ XPath.anywhere xPath
--  where
--  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.aceEditorXPath

--findMdQueryColumnCellsByHeading :: String -> SlamFeature (List Element)
--findMdQueryColumnCellsByHeading = findFollowingTableColumnCellsByHeading XPaths.mdQueryCellTitleXPath
--
--findMdQueryColumnCellsTextByHeading :: String -> SlamFeature (List String)
--findMdQueryColumnCellsTextByHeading s = findMdQueryColumnCellsByHeading s >>= T.traverse getText

