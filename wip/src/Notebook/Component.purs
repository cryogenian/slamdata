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

module Notebook.Component
  ( notebookComponent
  , initialState
  , NotebookQueryP()
  , NotebookStateP()
  , module Notebook.Component.Query
  , module Notebook.Component.State
  ) where

import Prelude

import Control.Bind ((=<<), join)

import Data.BrowserFeatures (BrowserFeatures())
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Data.Lens ((.~), (%~), preview)
import Data.List (fromList)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.These (These(..), theseLeft)

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Model.AccessType (isEditable)
import Model.CellId (CellId())
import Model.CellType (CellType(..), cellName, cellGlyph, autorun)
import Model.Port (Port())
import Model.Resource as R
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalInput())
import Notebook.Cell.Component (CellQueryP(), CellQuery(..), InnerCellQuery(), CellStateP(), _CellEvalQuery)
import Notebook.CellSlot (CellSlot(..))
import Notebook.Common (Slam(), forceRerender')
import Notebook.Component.Query
import Notebook.Component.State
import Quasar.Aff (loadNotebook)
import Render.Common (glyph, fadeWhen)
import Render.CssClasses as CSS

type NotebookQueryP = Coproduct NotebookQuery (ChildF CellSlot CellQueryP)
type NotebookStateP = InstalledState NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot

type NotebookHTML = ParentHTML CellStateP NotebookQuery CellQueryP Slam CellSlot
type NotebookDSL = ParentDSL NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot

initialState :: BrowserFeatures -> NotebookStateP
initialState fs = installedState $ initialNotebook fs

notebookComponent :: Component NotebookStateP NotebookQueryP Slam
notebookComponent = parentComponent' render eval peek

render :: NotebookState -> NotebookHTML
render state =
  H.div_
    $ fromList (map renderCell state.cells)
   <> if isEditable state.accessType then [newCellMenu state] else []

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
    , insertMenuItem Query
    , insertMenuItem Markdown
    , insertMenuItem Explore
    , insertMenuItem Search
    ]
  where
  insertMenuItem :: CellType -> NotebookHTML
  insertMenuItem cellType =
    H.li_
      [ H.button
          [ P.title (cellName cellType)
          , E.onClick $ E.input_ (AddCell cellType)
          , P.classes (fadeWhen $ not (state.isAddingCell))
          ]
          [ glyph (cellGlyph cellType) ]
      ]

eval :: Natural NotebookQuery NotebookDSL
eval (AddCell cellType next) = do
  let p = map (P.Resource <<< Rs.File <<< (Pt.rootDir Pt.</>))
          $ Pt.parseAbsFile "/demo/demo/flatViz" >>= Pt.sandbox Pt.rootDir
  modify (addCell cellType Nothing)
  forceRerender'
  traceAnyA p
  updateCell zero { notebookPath: Nothing
                  , inputPort: p
                  , cellId: one
                  }
  traceAnyA "FOO"
  pure next

eval (RunActiveCell next) =
  (maybe (pure unit) runCell =<< gets (_.activeCellId)) $> next
eval (ToggleAddCellMenu next) = modify (_isAddingCell %~ not) $> next
eval (LoadResource fs res next) = do
  model <- liftH $ liftAff' $ loadNotebook res
  modify $ const $ fromModel fs model
  modify (_path .~ R.resourceDir res)
  pure next
eval (SetName name next) = modify (_name .~ That name) $> next
eval (SetAccessType aType next) = modify (_accessType .~ aType) $> next
eval (GetNameToSave continue) = map continue $ gets $ _.name >>> theseLeft
eval (SetViewingCell mbcid next) = modify (_viewingCell .~ mbcid) $> next

peek :: forall a. ChildF CellSlot CellQueryP a -> NotebookDSL Unit
peek (ChildF (CellSlot cellId) q) = coproduct (peekCell cellId) (peekCellInner cellId) q

peekCell :: forall a. CellId -> CellQuery a -> NotebookDSL Unit
peekCell cellId q = case q of
  RunCell _ -> runCell cellId
  RefreshCell _ -> runCell <<< flip findRoot cellId =<< get
  TrashCell _ -> do
    descendants <- findDescendants <$> get <*> pure cellId
    modify (removeCells $ S.insert cellId descendants)
  CreateChildCell cellType _ -> do
    modify (addCell cellType (Just cellId))
    if autorun cellType
      then do
      vals <- gets _.values
      updateCell (cellId + one ) { notebookPath: Nothing
                                 , inputPort: M.lookup cellId vals
                                 , cellId: cellId + one
                                 }
      else pure unit
  ShareCell _ -> pure unit
  _ -> pure unit

peekCellInner :: forall a. CellId -> ChildF Unit InnerCellQuery a -> NotebookDSL Unit
peekCellInner cellId (ChildF _ q) =
  case preview _CellEvalQuery q of
    Just (NotifyRunCell _) -> runCell cellId
    _ -> pure unit

import Model.Port as P
import Data.Path.Pathy as Pt
import Model.Resource as Rs
import Debug.Trace
runCell :: CellId -> NotebookDSL Unit
runCell cellId = do
  st <- get
  traceAnyA $ findParent st cellId
  case findParent st cellId of
    Nothing -> do

      let p = map (P.Resource <<< Rs.File <<< (Pt.rootDir Pt.</>))
              $ Pt.parseAbsFile "/demo/demo/flatViz" >>= Pt.sandbox Pt.rootDir
      traceAnyA p
      updateCell cellId
        { notebookPath: notebookPath st
        , inputPort: p
        , cellId: cellId
        }
    Just parent -> do
      traceAnyA "Just"
      case getCurrentValue st parent of
        Just inputPort ->
          updateCell cellId
            { inputPort: Just inputPort
            , notebookPath: notebookPath st
            , cellId: cellId
            }
        Nothing -> pure unit

updateCell :: CellId -> CellEvalInput -> NotebookDSL Unit
updateCell cellId input = do
  traceAnyA input
  result <- query (CellSlot cellId) $ left $ request (UpdateCell input)
  maybe (pure unit) (runCellDescendants cellId) $ join result

runCellDescendants :: CellId -> Port -> NotebookDSL Unit
runCellDescendants cellId value = do
  st <- get
  let
    children = findChildren st cellId
    cellEvalInput =
      { notebookPath: notebookPath st
      , inputPort: Just value
      , cellId: cellId
      }
  traverse_ (flip updateCell cellEvalInput) children
