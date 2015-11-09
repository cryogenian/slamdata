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

module Notebook.Cell.Markdown.Editor.Component
  ( markdownEditorComponent
  , module Notebook.Cell.Markdown.Editor.Component.Query
  , module Notebook.Cell.Markdown.Editor.Component.State
  ) where

import Prelude

import Data.Lens.Prism.Coproduct as C
import Data.Lens.Prism.Either as E
import Data.Maybe (Maybe(..), fromMaybe)

import Halogen
import Halogen.HTML.CSS.Indexed as P
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Css.Size (px)
import Css.Geometry (height)

import Ace.Halogen.Component (AceQuery(..), AceState(), aceConstructor)
import Text.Markdown.SlamDown.Parser (parseMd)

import Render.CssClasses as CSS

import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..))
import Notebook.Cell.Component (CellStateP(), CellQueryP(), makeEditorCellComponent, makeQueryPrism, _MarkdownState, _MarkdownQuery)
import Notebook.Cell.Markdown.Editor.Component.Query
import Notebook.Cell.Markdown.Editor.Component.State
import Notebook.Cell.Port (Port(..))
import Notebook.Common (Slam())

markdownEditorComponent :: Component CellStateP CellQueryP Slam
markdownEditorComponent = makeEditorCellComponent
  { name: "Markdown"
  , glyph: B.glyphiconEdit
  , component: parentComponent render eval
  , initialState: installedState unit
  , _State: _MarkdownState <<< E._Left
  , _Query: makeQueryPrism (_MarkdownQuery <<< C._Left)
  }

render :: Unit -> ParentHTML AceState CellEvalQuery AceQuery Slam Unit
render _ =
  H.div
    [ P.class_ CSS.aceContainer, P.style (height (px 160.0)) ]
    [ H.Slot (aceConstructor unit Nothing) ]

eval :: Natural CellEvalQuery (ParentDSL Unit AceState CellEvalQuery AceQuery Slam Unit)
eval (EvalCell _ k) = do
  content <- fromMaybe "" <$> query unit (request GetText)
  pure $ k
    { output: Just $ SlamDown (parseMd content)
    , messages: []
    }
