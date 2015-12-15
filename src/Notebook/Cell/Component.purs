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

import Control.Bind (join, (=<<))
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling (producerToStallingProducer)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Free (liftF)

import Data.Date as Date
import Data.Either (Either(..))
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Coproduct (left)
import Data.Lens (PrismP(), review, preview, clonePrism, (.~), (%~), (^.))
import Data.Maybe (Maybe(..), maybe)
import Data.Path.Pathy as Path
import Data.Visibility (Visibility(..), toggleVisibility)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Query.HalogenF (HalogenFP(..))
import Halogen.Themes.Bootstrap3 as B

import DOM.Timer (interval, clearInterval)

import Render.Common (row, row', glyph)
import Render.CssClasses as CSS

import Model.AccessType (AccessType(..))
import Model.CellType (CellType(..), cellGlyph, cellName)
import Model.Port (Port(..), _Resource)
import Model.Resource (_filePath)

import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), prepareCellEvalInput)
import Notebook.Cell.Component.Def
import Notebook.Cell.Component.Query
import Notebook.Cell.Component.Render (CellHTML(), header, statusBar)
import Notebook.Cell.Component.State
import Notebook.Cell.RunState (RunState(..))
import Notebook.Common (Slam(), liftAff'', liftEff'')

-- | Type synonym for the full type of a cell component.
type CellComponent = Component CellStateP CellQueryP Slam
type CellDSL = ParentDSL CellState AnyCellState CellQuery InnerCellQuery Slam Unit
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
          -- Do we really need `row` here? Won't `H.div_` work?
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
                [ H.text (resLabel cs.input)
                , H.ul [ P.class_ CSS.nextCellList ] (nextCellButtons cs.output)
                ]
            , H.div
                [ P.class_ CSS.cellOutputResult ]
                [ H.slot unit \_ -> { component: component, initialState: initialState } ]
            ]
        ]

  resLabel :: Maybe Port -> String
  resLabel p = maybe "" (\p -> Path.runFileName (Path.fileName p) ++ " :=") $ preview (_Resource <<< _filePath) =<< p

  nextCellButtons :: Maybe Port -> Array (CellHTML)
  nextCellButtons Nothing = []
  nextCellButtons (Just p) = case p of
    VarMap _ ->
      [ nextCellButton Query ]
    Resource _ ->
      [ nextCellButton Query
      , nextCellButton Search
      , nextCellButton Viz
      ]
    _ -> []

  nextCellButton :: CellType -> CellHTML
  nextCellButton cellType =
    H.li_
      [ H.button
          [ P.title (cellName cellType)
          , E.onClick $ E.input_ (CreateChildCell cellType)
          ]
          [ glyph (cellGlyph cellType) ]
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

  eval :: Natural CellQuery CellDSL
  eval (RunCell next) = pure next
  eval (UpdateCell input k) = do
    liftAff'' =<< gets (^. _tickStopper)
    tickStopper <- startInterval
    modify (_tickStopper .~ tickStopper)
    cachingEnabled <- gets _.cachingEnabled
    let input' = prepareCellEvalInput cachingEnabled input
    modify (_input .~ input'.inputPort)
    result <- query unit (left (request (EvalCell input')))
    liftAff'' tickStopper
    modify
      $ (_runState %~ finishRun)
      <<< (_output .~ (_.output =<< result))
    maybe (liftF HaltHF) (pure <<< k <<< _.output) result
  eval (RefreshCell next) = pure next
  eval (TrashCell next) = pure next
  eval (CreateChildCell _ next) = pure next
  eval (ToggleCollapsed next) =
    modify (_isCollapsed %~ not) $> next
  eval (ToggleMessages next) =
    modify (_messageVisibility %~ toggleVisibility) $> next
  eval (ToggleCaching next) =
    modify (_cachingEnabled %~ not) $> next
  eval (ShareCell next) = pure next
  eval (Tick elapsed next) =
    modify (_runState .~ RunElapsed elapsed) $> next
  eval (GetOutput k) = k <$> gets (_.output)

-- | Starts a timer running on an interval that passes Tick queries back to the
-- | component, allowing the runState to be updated with a timer.
-- |
-- | The returned value is an action that will stop the timer running when
-- | processed.
startInterval :: CellDSL (Slam Unit)
startInterval = do
  ref <- liftEff'' $ newRef Nothing
  start <- liftEff'' Date.now
  modify (_runState .~ RunElapsed zero)

  subscribe' $ EventSource $ producerToStallingProducer $ produce \emit -> do
    i <- interval 1000 $ emit <<< Left <<< action <<< Tick =<< liftEff do
      now <- Date.now
      pure $ on (-) Date.toEpochMilliseconds now start
    writeRef ref (Just i)

  pure $ maybe (pure unit) (liftEff <<< clearInterval) =<< liftEff (readRef ref)

-- | Update the `RunState` from its current value to `RunFinished`.
finishRun :: RunState -> RunState
finishRun RunInitial = RunElapsed zero
finishRun (RunElapsed ms) = RunFinished ms
finishRun (RunFinished ms) = RunFinished ms
