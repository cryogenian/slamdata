module Notebook.Cell.Component
  ( makeEditorCellComponent
  , makeResultsCellComponent
  , module Notebook.Cell.Component.Def
  , module Notebook.Cell.Component.Query
  , module Notebook.Cell.Component.State
  ) where

import Prelude

import Control.Bind ((=<<), join)
import Control.Monad.Free (liftF)

import Data.Functor (($>))
import Data.Functor.Coproduct (coproduct, left, right)
import Data.Lens (PrismP(), review, preview, clonePrism)
import Data.Maybe (Maybe(..), maybe)
import Data.Visibility (Visibility(..), toggleVisibility)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (row, row')
import Render.CssClasses as CSS

import Notebook.AccessType (AccessType(..))
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..))
import Notebook.Cell.Component.Def
import Notebook.Cell.Component.Query
import Notebook.Cell.Component.Render (CellHTML(), header, statusBar)
import Notebook.Cell.Component.State
import Notebook.Common (Slam())

-- | Constructs a cell component for an editor-style cell.
makeEditorCellComponent
  :: forall s f
   . EditorDef s f
  -> Component CellStateP CellQueryP Slam
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
   . ResultsDef s f
  -> Component CellStateP CellQueryP Slam
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
   . Object (CellProps s f r)
  -> (Component AnyCellState InnerCellQuery Slam -> AnyCellState -> CellState -> CellHTML)
  -> Component CellStateP CellQueryP Slam
makeCellComponentPart def render =
  parentComponent (render component initialState) eval
  where

  _State :: PrismP AnyCellState s
  _State = clonePrism def._State

  _Query :: forall a. PrismP (AnyCellQuery a) (f a)
  _Query = clonePrism def._Query

  component :: Component AnyCellState InnerCellQuery Slam
  component = transform
    (review _State)
    (preview _State)
    (coproduct right (left <<< review _Query))
    (coproduct (map right <<< preview _Query) (Just <<< left))
    def.component

  initialState :: AnyCellState
  initialState = review _State def.initialState

  eval :: Natural CellQuery (ParentDSL CellState AnyCellState CellQuery InnerCellQuery Slam Unit)
  eval (RunCell next) = pure next
  eval (UpdateCell input k) =
    maybe (liftF HaltHF) (pure <<< k <<< _.output) =<< query unit (right (request (EvalCell input)))
  eval (RefreshCell next) = pure next
  eval (TrashCell next) = pure next
  eval (CreateChildCell _ next) = pure next
  eval (ToggleCollapsed next) =
    modify (\st -> st { isCollapsed = not st.isCollapsed }) $> next
  eval (ToggleMessages next) =
    modify (\st -> st { messageVisibility = toggleVisibility st.messageVisibility }) $> next
  eval (ShareCell next) = pure next
