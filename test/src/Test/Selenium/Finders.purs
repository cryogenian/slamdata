module Test.Selenium.Finders where

import Prelude

import Control.Apply ((*>))
import Control.Alt ((<|>))
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.List (elemIndex)
import Data.Maybe (Maybe(..), maybe)
import Selenium (showLocator)
import Selenium.Monad (getAttribute, getText, byXPath, byId, tryRepeatedlyTo, findElements, findExact)
import Selenium.Types (Element(), Locator())
import Test.Selenium.Common (attrFail)
import Test.Selenium.Log (warnMsg)
import Test.Selenium.Monad (Check(), findAtLeast, findSingle)

import qualified Data.Traversable (traverse) as T

findElementIndexByText :: String -> Locator -> Check Int
findElementIndexByText text locator = do
  elements <- tryRepeatedlyTo $ findAtLeast 1 locator
  texts <- T.traverse getText elements
  case elemIndex text texts of
    Just i -> return i
    Nothing -> throwError $ error $ e
      where
      e = "Expected an element with xPath " ++ showLocator locator
                                            ++ " to have text "
                                            ++ show text ++ "."

findElementIdByLabelText :: String -> Check String
findElementIdByLabelText text = getLocator >>= \locator -> go locator
                                           >>= maybe (attrFail locator attr) pure
    where
    attr = "for"
    getLocator = byXPath $ "//label[text()='" ++ text ++ "']"
    go locator = tryRepeatedlyTo $ findSingle locator >>= flip getAttribute attr

findElementByLabelText :: String -> Check Element
findElementByLabelText text = findElementIdByLabelText text >>= byId >>= findSingle

findSingleGracefully :: Locator -> Check Element
findSingleGracefully loc = findSingle loc <|> (warn *> findExact loc)
  where
  warn = warnMsg $
    "Warning, found more than one element with locator: "
    ++ showLocator loc
    ++ ". Are we sure this is the right one?"
