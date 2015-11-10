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

module Notebook.Cell.Component
  ( CellComponent()
  , makeEditorCellComponent
  , makeResultsCellComponent
  , module Notebook.Cell.Component.Def
  , module Notebook.Cell.Component.Query
  , module Notebook.Cell.Component.State
  ) where

import Prelude

import Control.Bind ((=<<), join)
import Control.Monad.Free (liftF)

import Data.Functor (($>))
import Data.Functor.Coproduct (left)
import Data.Lens (PrismP(), review, preview, clonePrism)
import Data.Maybe (maybe)
import Data.Visibility (Visibility(..), toggleVisibility)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (row, row')
import Render.CssClasses as CSS

import Model.AccessType (AccessType(..))
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..))
import Notebook.Cell.Component.Def
import Notebook.Cell.Component.Query
import Notebook.Cell.Component.Render (CellHTML(), header, statusBar)
import Notebook.Cell.Component.State
import Notebook.Common (Slam())

-- | Type synonym for the full type of a cell component.
type CellComponent = Component CellStateP CellQueryP Slam

-- | Constructs a cell component for an editor-style cell.
makeEditorCellComponent
  :: forall s f
   . EditorCellDef s f
  -> CellComponent
makeEditorCellComponent def = makeCellComponentPart def render
  where
  render :: Component AnyCellState InnerCellQuery Slam -> AnyCellState -> CellState -> CellHTML
  render component initialState cs =
    if cs.visibility == Invisible || cs.accessType == ReadOnly
    then H.text ""
    else
      H.div
        [ P.classes $ join [containerClasses, collapsedClass] ]
        [ header def cs
        , row [ H.slot unit \_ -> { component: component, initialState: initialState } ]
        , statusBar cs.hasResults cs
        ]
    where
    collapsedClass = if cs.isCollapsed then [CSS.collapsed] else []

-- | Constructs a cell component for an results-style cell.
makeResultsCellComponent
  :: forall s f
   . ResultsCellDef s f
  -> CellComponent
makeResultsCellComponent def = makeCellComponentPart def render
  where
  render :: Component AnyCellState InnerCellQuery Slam -> AnyCellState -> CellState -> CellHTML
  render component initialState cs =
    if cs.visibility == Invisible
    then H.text ""
    else
      H.div
        [ P.classes containerClasses ]
        [ row' [CSS.cellOutput]
            [ H.div
                [ P.class_ CSS.cellOutputLabel ]
                []
            , H.div
                [ P.class_ CSS.cellOutputResult ]
                [ H.slot unit \_ -> { component: component, initialState: initialState } ]
            ]
        ]

containerClasses :: Array (H.ClassName)
containerClasses = [B.containerFluid, CSS.notebookCell, B.clearfix]

-- | Constructs a cell component from a record with the necessary properties and
-- | a render function.
makeCellComponentPart
  :: forall s f r
   . Object (CellDefProps s f r)
  -> (Component AnyCellState InnerCellQuery Slam -> AnyCellState -> CellState -> CellHTML)
  -> CellComponent
makeCellComponentPart def render =
  parentComponent (render component initialState) eval
  where

  _State :: PrismP AnyCellState s
  _State = clonePrism def._State

  _Query :: forall a. PrismP (InnerCellQuery a) (f a)
  _Query = clonePrism def._Query

  component :: Component AnyCellState InnerCellQuery Slam
  component = transform (review _State) (preview _State) (review _Query) (preview _Query) def.component

  initialState :: AnyCellState
  initialState = review _State def.initialState

  eval :: Natural CellQuery (ParentDSL CellState AnyCellState CellQuery InnerCellQuery Slam Unit)
  eval (RunCell next) = pure next
  eval (UpdateCell input k) =
    maybe (liftF HaltHF) (pure <<< k <<< _.output) =<< query unit (left (request (EvalCell input)))
  eval (RefreshCell next) = pure next
  eval (TrashCell next) = pure next
  eval (CreateChildCell _ next) = pure next
  eval (ToggleCollapsed next) =
    modify (\st -> st { isCollapsed = not st.isCollapsed }) $> next
  eval (ToggleMessages next) =
    modify (\st -> st { messageVisibility = toggleVisibility st.messageVisibility }) $> next
  eval (ShareCell next) = pure next
