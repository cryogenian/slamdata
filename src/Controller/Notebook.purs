module Controller.Notebook (handleMenuSignal) where

import Data.Maybe
import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Monad.Eff.Class
import Model.Notebook.Menu (
  MenuNotebookSignal(..), 
  MenuEditSignal(..),
  MenuInsertSignal(..),
  MenuCellSignal(..),
  MenuHelpSignal(..),
  MenuSignal(..))
import Model.Notebook (I())
import Control.Inject1 (prj)
import Debug.Foreign -- mark for grep -nr to not remove. mocking handlers


handleMenuNotebook :: forall e. MenuNotebookSignal -> I e
handleMenuNotebook signal = do 
  liftEff $ fprint signal
  empty

handleMenuEdit :: forall e. MenuEditSignal -> I e
handleMenuEdit signal = do
  liftEff $ fprint signal
  empty

handleMenuInsert :: forall e. MenuInsertSignal -> I e
handleMenuInsert signal = do
  liftEff $ fprint signal
  empty

handleMenuCell :: forall e. MenuCellSignal -> I e
handleMenuCell signal = do
  liftEff $ fprint signal
  empty

handleMenuHelp :: forall e. MenuHelpSignal -> I e
handleMenuHelp signal = do
  liftEff $ fprint signal
  empty

handleMenuSignal :: forall e. MenuSignal -> I e
handleMenuSignal signal =
  maybe empty id $
  (handleMenuNotebook <$> prj signal)
  <|>
  (handleMenuEdit <$> prj signal)
  <|>
  (handleMenuInsert <$> prj signal)
  <|>
  (handleMenuCell <$> prj signal)
  <|>
  (handleMenuHelp <$> prj signal)
