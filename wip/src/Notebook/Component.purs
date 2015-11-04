module Notebook.Component
  ( notebookComponent
  , module Notebook.Component.Query
  , module Notebook.Component.State
  ) where

import Prelude

import Control.Bind ((=<<))

import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Data.List (fromList)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (glyph, fadeWhen)
import Render.CssClasses as CSS

import Notebook.Cell.CellType (CellType(..))
import Notebook.Cell.Component (CellQuery(..), CellQueryP(), CellStateP())
import Notebook.Cell.Port (Port(..))
import Notebook.CellSlot (CellSlot(..), CellId())
import Notebook.Common (Slam())
import Notebook.Component.Query
import Notebook.Component.State

type NotebookQueryP = Coproduct NotebookQuery (ChildF CellSlot CellQueryP)
type NotebookStateP = InstalledState NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot

type NotebookHTML = ParentHTML CellStateP NotebookQuery CellQueryP Slam CellSlot
type NotebookDSL = ParentDSL NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot

notebookComponent :: Component NotebookStateP NotebookQueryP Slam
notebookComponent = parentComponent' render eval peek

render :: NotebookState -> NotebookHTML
render state =
  H.div_
    $ fromList (map renderCell state.cells)
   <> if state.editable then [newCellMenu state] else []

renderCell :: CellDef -> NotebookHTML
renderCell def = H.Slot def.ctor

newCellMenu :: NotebookState -> NotebookHTML
newCellMenu state =
  H.ul
    [ P.class_ CSS.newCellMenu ]
    [ H.li_
        [ H.button
            [ P.classes [B.btnLg, B.btnLink]
            , E.onClick (E.input_ ToggleAddCellMenu)
            , P.title "Insert new cell"
            ]
            [ glyph
                if state.isAddingCell
                then B.glyphiconMinus
                else B.glyphiconPlus
            ]
        ]
    , insertMenuItem "Query" Query B.glyphiconHdd
    , insertMenuItem "Markdown" Markdown B.glyphiconEdit
    , insertMenuItem "Explore" Explore B.glyphiconEyeOpen
    , insertMenuItem "Search" Search B.glyphiconSearch
    ]
  where
  insertMenuItem :: String -> CellType -> H.ClassName -> NotebookHTML
  insertMenuItem title cellType cls =
    H.li_
      [ H.button
          [ P.title title
          , E.onClick $ E.input_ (AddCell cellType)
          , P.classes (fadeWhen $ not (state.isAddingCell))
          ]
          [ glyph cls ]
      ]


eval :: Natural NotebookQuery NotebookDSL
eval (AddCell cellType next) = modify (\st -> addCell st cellType Nothing) $> next
eval (RunActiveCell next) = (maybe (pure unit) runCell =<< gets (_.activeCellId)) $> next
eval (ToggleAddCellMenu next) = modify (\st -> st { isAddingCell = not st.isAddingCell }) $> next

peek :: forall a. ChildF CellSlot CellQueryP a -> NotebookDSL Unit
peek (ChildF slot q) = coproduct (peekCell slot) (const (pure unit)) q

peekCell :: forall a. CellSlot -> CellQuery a -> NotebookDSL Unit
peekCell (CellSlot cellId) q = case q of
  RunCell _ -> runCell cellId
  RefreshCell _ -> runCell <<< flip findRoot cellId =<< get
  TrashCell _ -> do
    descendants <- findDescendants <$> get <*> pure cellId
    modify (flip removeCells $ S.insert cellId descendants)
  CreateChildCell cellType _ -> modify $ \st -> addCell st cellType (Just cellId)
  ShareCell _ -> pure unit -- TODO: open share modal
  _ ->
    pure unit

runCell :: CellId -> NotebookDSL Unit
runCell cellId = do
  st <- get
  case findParent st cellId of
    Nothing -> updateCell cellId Closed
    Just parent -> maybe (pure unit) (updateCell cellId) $ getCurrentValue st parent

updateCell :: CellId -> Port -> NotebookDSL Unit
updateCell cellId value = do
  result <- query (CellSlot cellId) $ left $ request (UpdateCell value)
  maybe (pure unit) (runCellDescendants cellId) result

runCellDescendants :: CellId -> Port -> NotebookDSL Unit
runCellDescendants cellId value = do
  children <- findChildren <$> get <*> pure cellId
  traverse_ (flip updateCell value) children
