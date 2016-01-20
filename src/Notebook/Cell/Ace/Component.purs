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
  , AceDSL()
  , AceHTML()
  , AceSetup()
  , module Notebook.Cell.Ace.Component.Query
  , module Notebook.Cell.Ace.Component.State
  ) where

import Prelude

import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Halogen.Component
  ( AceQuery(SetText, GetText), AceState(), Autocomplete(..)
  , aceConstructor)
import Ace.Types (Editor())
import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (either)
import Data.Functor (($>))
import Data.Functor.Eff (liftEff)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Notebook.Cell.Ace.Component.Query
import Notebook.Cell.Ace.Component.State
import Notebook.Cell.CellType (AceMode(), aceMode, aceCellName, aceCellGlyph)
import Notebook.Cell.Common.EvalQuery
  (CellEvalQuery(..), CellEvalResult(), CellEvalInput())
import Notebook.Cell.Component
  ( CellStateP(), CellQueryP(), makeEditorCellComponent, makeQueryPrism
  , _AceState, _AceQuery)
import Notebook.Cell.Port as P
import Notebook.Common (Slam())
import Render.CssClasses as CSS

type AceDSL =
  ParentDSL Unit AceState CellEvalQuery AceQuery Slam Unit
type AceHTML =
  ParentHTML AceState CellEvalQuery AceQuery Slam Unit
type AceEvaluator =
  CellEvalInput -> String -> AceDSL CellEvalResult
type AceSetup =
  P.Port -> AceDSL Unit

type AceConfig =
  { mode :: AceMode
  , evaluator :: AceEvaluator
  , setup :: AceSetup
  }


aceComponent
  :: AceConfig -> Component CellStateP CellQueryP Slam
aceComponent {mode, evaluator, setup} = makeEditorCellComponent
  { name: aceCellName mode
  , glyph: aceCellGlyph mode
  , component: parentComponent render eval
  , initialState: installedState unit
  , _State: _AceState
  , _Query: makeQueryPrism _AceQuery
  }

  where
  render :: Unit -> AceHTML
  render _ =
    H.div
      [ P.classes [CSS.cellInput, CSS.aceContainer]
      ]
      [ H.Slot (aceConstructor unit aceSetup (Just Live) ) ]

  aceSetup :: Editor -> Slam Unit
  aceSetup editor = liftEff do
    Editor.setMinLines 4 editor
    Editor.setMaxLines 10000 editor
    Editor.setAutoScrollEditorIntoView true editor
    Editor.setTheme "ace/theme/chrome" editor
    Editor.setEnableLiveAutocompletion true editor
    Editor.setEnableBasicAutocompletion true editor
    session <- Editor.getSession editor
    Session.setMode (aceMode mode) session


  eval :: Natural CellEvalQuery AceDSL
  eval (NotifyRunCell next) = pure next
  eval (EvalCell info k) = do
    content <- fromMaybe "" <$> query unit (request GetText)
    result <- evaluator info content
    pure $ k result
  eval (SetupCell port next) = setup port $> next
  eval (Save k) = do
    content <- fromMaybe "" <$> query unit (request GetText)
    pure $ k (encodeJson content)
  eval (Load json next) = do
    let text = fromMaybe "" $ either (const Nothing) id $ decodeJson json
    query unit $ action (SetText text)
    pure next
