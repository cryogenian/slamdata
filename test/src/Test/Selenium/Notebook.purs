module Test.Selenium.Notebook (test) where

import Prelude
import Control.Apply ((*>))
import Data.Maybe (maybe)
import Test.Config
import Selenium.ActionSequence
import Selenium.MouseButton
import Test.Selenium.Common
import Test.Selenium.Monad
import Test.Selenium.Log
import Driver.File.Routing (Routes(..), routing)
import Test.Selenium.File hiding (test)
import Utils.Log

-- | We are in notebook after `setUp`. No need to test if notebook created
-- | it's tested in `Test.Selenium.File`
setUp :: Check Unit
setUp = do 
  home
  mountDatabase
  enterMount
  createNotebookAndThen $ pure unit
  
test :: Check Unit
test = do
  setUp
  notebookLoaded
