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

module Notebook.Cell.Ace.Component
  ( aceComponent
  , AceEvaluator()
  , module Notebook.Cell.Ace.Component.Query
  , module Notebook.Cell.Ace.Component.State
  ) where

import Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (either)
import Data.Functor.Aff (liftAff)
import Data.Functor.Eff (liftEff)
import Data.Maybe (Maybe(..), fromMaybe)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Ace.Types (Editor())
import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceQuery(..), AceState(), aceConstructor)

import Render.CssClasses as CSS

import Notebook.Cell.CellType (AceMode(), aceMode, aceCellName, aceCellGlyph)

import Notebook.Cell.Ace.Component.Query
import Notebook.Cell.Ace.Component.State
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalResult(), CellEvalInput())
import Notebook.Cell.Component (CellStateP(), CellQueryP(), makeEditorCellComponent, makeQueryPrism, _AceState, _AceQuery)
import Notebook.Common (Slam())

type AceEvaluator = CellEvalInput -> String -> Slam CellEvalResult

aceComponent :: AceMode -> AceEvaluator -> Component CellStateP CellQueryP Slam
aceComponent cellType run = makeEditorCellComponent
  { name: aceCellName cellType
  , glyph: aceCellGlyph cellType
  , component: parentComponent render eval
  , initialState: installedState unit
  , _State: _AceState
  , _Query: makeQueryPrism _AceQuery
  }

  where

  render :: Unit -> ParentHTML AceState CellEvalQuery AceQuery Slam Unit
  render _ =
    H.div
      [ P.classes [CSS.cellInput, CSS.aceContainer] ]
      [ H.Slot (aceConstructor unit aceSetup Nothing) ]

  aceSetup :: Editor -> Slam Unit
  aceSetup editor = liftEff do
    Editor.setMinLines 4 editor
    Editor.setMaxLines 10000 editor
    Editor.setAutoScrollEditorIntoView true editor
    Editor.setTheme "ace/theme/chrome" editor
    session <- Editor.getSession editor
    Session.setMode (aceMode cellType) session

  eval :: Natural CellEvalQuery (ParentDSL Unit AceState CellEvalQuery AceQuery Slam Unit)
  eval (NotifyRunCell next) = pure next
  eval (EvalCell info k) = do
    content <- fromMaybe "" <$> query unit (request GetText)
    result <- liftAff $ run info content
    pure $ k result
  eval (SetupCell _ next) = pure next
  eval (Save k) = do
    content <- fromMaybe "" <$> query unit (request GetText)
    pure $ k (encodeJson content)
  eval (Load json next) = do
    let text = fromMaybe "" $ either (const Nothing) id $ decodeJson json
    query unit $ action (SetText text)
    pure next
