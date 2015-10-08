module Test.Selenium.Notebook.Markdown.Finders where

import Prelude

import Test.Selenium.Monad (Check(), byAriaLabel)
import Test.Selenium.Finders (findElementIndexByText)
import Data.List (List())
import Selenium.Monad (byXPath, tryRepeatedlyTo, getText, findElements)
import Selenium.Combinators (tryToFind)
import Selenium.Types (Element())

import qualified Data.Traversable (traverse, sequence) as T
import qualified Data.Foldable (sequence_, traverse_) as F

findMdField :: Check Element
findMdField = tryToFind $ byXPath xPath
  where
  xPath = "//*[text()='Markdown']/following::*[contains(@class, 'ace_editor')]"

findMdPlayButton :: Check Element
findMdPlayButton = tryToFind $ byXPath xPath
  where
  xPath = "//*[text()='Markdown']/following::*[@aria-label='Play']"

findMdQueryPlayButton :: Check Element
findMdQueryPlayButton = tryToFind $ byXPath xPath
  where
  xPath = "//*[text()='Markdown']/following::*[text()='Query']/following::*[@aria-label='Play']"

findCreateMdQueryCellButton :: Check Element
findCreateMdQueryCellButton = tryToFind $ byAriaLabel "Query using fields"

findMdQueryField :: Check Element
findMdQueryField = tryToFind $ byXPath xPath
  where
  xPath = "//*[text()='Markdown']/following::*[text()='Query']/following::*[contains(@class, 'ace_editor')]"

findMdQueryColumnCellsByIndex :: Int -> Check (List Element)
findMdQueryColumnCellsByIndex index = tryRepeatedlyTo $ byXPath xPath >>= findElements
  where
  xPath = "//*[text()='Markdown']/following::*[text()='Query']/following::tbody/tr/td[" ++ show index ++ "]"

findMdQueryColumnCellsByHeading :: String -> Check (List Element)
findMdQueryColumnCellsByHeading heading =
  byXPath xPath >>= findElementIndexByText heading >>= findMdQueryColumnCellsByIndex
    where
    xPath = "//*[text()='Markdown']/following::*[text()='Query']/following::thead/tr/td"

findMdQueryColumnCellsTextByHeading :: String -> Check (List String)
findMdQueryColumnCellsTextByHeading s = findMdQueryColumnCellsByHeading s >>= T.traverse getText
