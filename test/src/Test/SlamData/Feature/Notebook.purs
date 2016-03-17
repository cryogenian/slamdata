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

module Test.SlamData.Feature.Notebook (test) where

import Prelude
import Test.SlamData.Feature.Monad (SlamFeature())
--import Test.SlamData.Feature.Monad (createTestDirs)
import Test.SlamData.Feature.Notebook.Markdown as Markdown
import Test.SlamData.Feature.Notebook.Search as Search
import Test.SlamData.Feature.Notebook.FlexibleVisualation as FlexibleVisualization

test :: SlamFeature Unit
test = do
  Search.test
  Markdown.test
  FlexibleVisualization.test
