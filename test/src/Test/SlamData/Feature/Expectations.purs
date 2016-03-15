module Test.SlamData.Feature.Expectations where

import Prelude
import Test.Feature (expectPresented, expectNotPresented)
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.XPaths as XPaths

cellsInTableColumnInLastCellToEq :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCellToEq =
  cellsInTableColumnInLastCell XPath.tdWithThAndTextEq

cellsInTableColumnInLastCellToContain :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCellToContain =
  cellsInTableColumnInLastCell XPath.tdWithThAndTextContaining

cellsInTableColumnInLastCellToNotEq :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCellToNotEq =
  cellsInTableColumnInLastCell XPath.tdWithThAndTextNotEq

cellsInTableColumnInLastCellToBeGT :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCellToBeGT =
  cellsInTableColumnInLastCell XPath.tdWithThAndTextGT

cellsInTableColumnInLastCellToBeLT :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCellToBeLT =
  cellsInTableColumnInLastCell XPath.tdWithThAndTextLT

cellsInTableColumnInLastCellToEqOneOf :: Int -> String -> Array String -> SlamFeature Unit
cellsInTableColumnInLastCellToEqOneOf =
  cellsInTableColumnInLastCell XPath.tdWithThAndTextEqOneOf

cellsInTableColumnInLastCellToNotEqOneOf :: Int -> String -> Array String -> SlamFeature Unit
cellsInTableColumnInLastCellToNotEqOneOf =
  cellsInTableColumnInLastCell XPath.tdWithThAndTextNotEqOneOf

cellsInTableColumnInLastCell
  :: forall a
  .  (String -> String -> a -> String)
  -> Int
  -> String
  -> a
  -> SlamFeature Unit
cellsInTableColumnInLastCell f i headerText xs = do
  expectPresented $ XPath.index tdXPath i
  expectNotPresented $ XPath.index trXPath (i + 1)
  where
  trXPath = tableXPath ++ "/tbody/tr"
  thXPath = XPath.thWithExactText headerText
  tdXPath = f tableXPath thXPath xs
  tableXPath = XPath.last (XPath.anywhere XPaths.searchPlayButton) `XPath.following` "table"
