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
import Test.Selenium.Monad (Check())
import Test.Selenium.Notebook.Contexts (setUp)
import Test.Selenium.Notebook.Explore as Explore
import Test.Selenium.Notebook.Search as Search
import Test.Selenium.Notebook.Common as Common
import Test.Selenium.Common
import Test.Selenium.Notebook.Viz as Viz
import Test.Selenium.Notebook.Markdown as Markdown
import Test.Selenium.Notebook.Complex as Complex

test :: Check Unit
test = do
  setUp
  --Common.test
  Explore.test
  --Search.test
  --Viz.test
  Markdown.test
  --Complex.test
