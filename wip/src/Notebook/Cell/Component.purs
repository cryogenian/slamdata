module Notebook.Cell.Component
  ( makeCellComponent
  , CellQueryP()
  , CellStateP()
  , module Notebook.Cell.Component.Def
  , module Notebook.Cell.Component.Query
  , module Notebook.Cell.Component.State
  ) where

import Prelude

import Control.Monad.Free (Free(), liftF)

import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Lens (PrismP(), review, preview, clonePrism)
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Render.CssClasses as CSS

import Notebook.Cell.Component.Def
import Notebook.Cell.Component.Query
import Notebook.Cell.Component.Render
import Notebook.Cell.Component.State
import Notebook.Cell.Common.EditorQuery (CellEditorQuery(..))
import Notebook.Cell.Common.ResultsQuery (CellResultsQuery(..))
import Notebook.Common (Slam())

type CellQueryP = Coproduct CellQuery (ChildF CellPart InnerCellQuery)
type CellStateP = InstalledState CellState AnyCellState CellQuery InnerCellQuery Slam CellPart

makeCellComponent
  :: forall se fe sr fr
   . Def se fe sr fr
  -> Component CellStateP CellQueryP Slam
makeCellComponent def = parentComponent render eval
  where

  _StateE :: PrismP AnyCellState se
  _StateE = clonePrism def._StateE

  _StateR :: PrismP AnyCellState sr
  _StateR = clonePrism def._StateR

  _QueryE :: forall a. PrismP (AnyCellQuery a) (fe a)
  _QueryE = clonePrism def._QueryE

  _QueryR :: forall a. PrismP (AnyCellQuery a) (fr a)
  _QueryR = clonePrism def._QueryR

  editor' :: Component AnyCellState InnerCellQuery Slam
  editor' = transform
    (review _StateE)
    (preview _StateE)
    (coproduct (right <<< left) (left <<< review _QueryE))
    (coproduct (map right <<< preview _QueryE) (coproduct (Just <<< left) (const Nothing)))
    def.editor

  results' :: Component AnyCellState InnerCellQuery Slam
  results' = transform
    (review _StateR)
    (preview _StateR)
    (coproduct (right <<< right) (left <<< review _QueryR))
    (coproduct (map right <<< preview _QueryR) (coproduct (const Nothing) (Just <<< left)))
    def.results

  render :: CellState -> ParentHTML AnyCellState CellQuery InnerCellQuery Slam CellPart
  render cs
    | cs.isInvisible = H.text ""
    | otherwise =
        let editorPart =
              if not (cs.showEditor && cs.isNotebookEditable)
              then Nothing
              else Just $
                H.div
                  [ P.class_ CSS.vizCellEditor ]
                  [ H.slot EditorPart \_ -> { component: editor', initialState: review _StateE def.editorState } ]
            resultsPart =
              H.slot ResultsPart \_ -> { component: results', initialState: review _StateR def.resultsState }
        in container def cs editorPart resultsPart

  eval :: Natural CellQuery (ParentDSL CellState AnyCellState CellQuery InnerCellQuery Slam CellPart)
  eval (RunCell next) = pure next
  eval (UpdateCell input k) = do
    result <- query EditorPart $ right $ left $ request (RunInnerCell input)
    case result of
      Nothing -> halt
      Just result' -> do
        modify (_ { hasResults = true })
        query ResultsPart $ right $ right $ action (UpdateResults result')
        pure (k result')
  eval (RefreshCell next) = pure next
  eval (TrashCell next) = pure next
  eval (CreateChildCell _ next) = pure next
  eval (ToggleEditor next) =
    modify (\st -> st { showEditor = not st.showEditor }) $> next
  eval (ToggleMessages next) =
    modify (\st -> st { showMessages = not st.showMessages }) $> next
  eval (ShareCell next) = pure next

-- | Halt value used to raise errors when evaluating a query of the wrong value
-- | or when the component has a bad state value.
halt :: forall s f g a. Free (HalogenF s f g) a
halt = liftF HaltHF
