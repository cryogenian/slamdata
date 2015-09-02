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

module View.Notebook.Cell.Explore (exploreEditor, exploreOutput) where

import Prelude
import Controller.Notebook.Cell.Explore (runExplore)
import Model.Notebook.Cell (Cell(), _FileInput, _JTableContent, _content, _input, _output)
import Optic.Core
import View.Notebook.Cell.FileInput (fileInput)
import View.Notebook.Cell.JTableCell (renderJTableOutput)
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified View.Css as VC

exploreEditor :: forall e. Cell -> HTML e
exploreEditor cell = H.div [ A.class_ VC.exploreCellEditor ] $
                     fileInput (_content .. _FileInput) (\port -> (_input .~ port) .. (_output .~ port)) cell


exploreOutput :: forall e. Cell -> Array (HTML e)
exploreOutput = renderJTableOutput (_content .. _JTableContent) runExplore
