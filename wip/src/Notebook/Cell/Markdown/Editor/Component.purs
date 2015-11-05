module Notebook.Cell.Markdown.Editor.Component
  ( markdownEditorComponent
  , MarkdownEditorQueryP()
  , MarkdownEditorStateP()
  ) where

import Prelude

import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), fromMaybe)

import Halogen
import Halogen.HTML.CSS.Indexed as P
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Css.Size (px)
import Css.Geometry (height)

import Ace.Halogen.Component (AceQuery(..), AceState(), aceConstructor)
import Text.Markdown.SlamDown.Parser (parseMd)

import Render.CssClasses as CSS

import Notebook.Common (Slam())
import Notebook.Cell.ResultsValue (ResultsValue(..))
import Notebook.Cell.Common.EditorQuery (CellEditorQuery(..))

type MarkdownEditorQueryP = Coproduct CellEditorQuery (ChildF Unit AceQuery)
type MarkdownEditorStateP = InstalledState Unit AceState CellEditorQuery AceQuery Slam Unit

markdownEditorComponent :: Component MarkdownEditorStateP MarkdownEditorQueryP Slam
markdownEditorComponent = parentComponent render eval

render :: Unit -> ParentHTML AceState CellEditorQuery AceQuery Slam Unit
render _ =
  H.div
    [ P.class_ CSS.aceContainer, P.style (height (px 160.0)) ]
    [ H.Slot (aceConstructor unit Nothing) ]

eval :: Natural CellEditorQuery (ParentDSL Unit AceState CellEditorQuery AceQuery Slam Unit)
eval (EvalCell _ k) = do
  content <- fromMaybe "" <$> query unit (request GetText)
  -- TODO: output should be the VarMap
  pure $ k
    { output: Nothing
    , result: Just $ SlamDown (parseMd content)
    , messages: []
    }
