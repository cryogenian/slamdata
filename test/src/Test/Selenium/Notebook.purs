{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

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
