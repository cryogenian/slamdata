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
import Notebook.Cell.Component (CellStateP(), CellQueryP(), makeEditorCellComponent, _MarkdownState, _MarkdownQuery)
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
  , _Query: _MarkdownQuery <<< C._Left <<< C._Right
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
