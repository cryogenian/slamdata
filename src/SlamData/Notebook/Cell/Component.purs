{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook.Cell.Component
  ( CellComponent()
  , makeEditorCellComponent
  , makeResultsCellComponent
  , makeSingularCellComponent
  , module SlamData.Notebook.Cell.Component.Def
  , module SlamData.Notebook.Cell.Component.Query
  , module SlamData.Notebook.Cell.Component.State
  ) where

import Prelude

import Control.Bind (join, (=<<))
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling (producerToStallingProducer)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Free (liftF)
import Control.Monad.Aff (cancel)
import Control.Monad.Eff.Exception as Exn

import Data.Functor.Aff (liftAff)
import Data.Functor.Eff (liftEff)
import Data.Functor.Coproduct (coproduct)
import Data.Argonaut (jsonNull)
import Data.Date as Date
import Data.Either (Either(..))
import Data.Foldable as F
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Aff (liftAff)
import Data.Functor.Coproduct (left)
import Data.Functor.Eff (liftEff)
import Data.Lens (PrismP(), review, preview, clonePrism, (.~), (%~), (^.))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.Path.Pathy as Path
import Data.Visibility (Visibility(..), toggleVisibility)

import DOM.Timer (interval, clearInterval)

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Query.HalogenF (HalogenFP(..))
import Halogen.Themes.Bootstrap3 as B

import SlamData.FileSystem.Resource (_filePath)
import SlamData.Notebook.AccessType (AccessType(..))
import SlamData.Notebook.Cell.CellType (CellType(..), AceMode(..), cellGlyph, cellName)
import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), prepareCellEvalInput)
import SlamData.Notebook.Cell.Component.Def
import SlamData.Notebook.Cell.Component.Query
import SlamData.Notebook.Cell.Component.Render (CellHTML(), header, statusBar)
import SlamData.Notebook.Cell.Component.State
import SlamData.Notebook.Cell.Port (Port(..), _Resource)
import SlamData.Notebook.Cell.RunState (RunState(..))
import SlamData.Effects (Slam())
import SlamData.Render.Common (row', glyph, fadeWhen)
import SlamData.Render.CSS as CSS

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
  render
    :: Component AnyCellState InnerCellQuery Slam
    -> AnyCellState
    -> CellState
    -> CellHTML
  render =
    cellSourceRender
      def
      (\x -> x.visibility == Invisible || x.accessType == ReadOnly)
      (\x -> [statusBar x.hasResults x])

-- | Sometimes we don't need editor or results and whole cell can be expressed
-- | as one cell part
makeSingularCellComponent
  :: forall s f
   . EditorCellDef s f
  -> CellComponent
makeSingularCellComponent def = makeCellComponentPart def render
  where
  render
    :: Component AnyCellState InnerCellQuery Slam
    -> AnyCellState
    -> CellState
    -> CellHTML
  render =
    cellSourceRender
      def
      (\x -> x.visibility == Invisible)
      (const [])

cellSourceRender
  :: forall s f
   . EditorCellDef s f
  -> (CellState -> Boolean)
  -> (CellState -> Array CellHTML)
  -> Component AnyCellState InnerCellQuery Slam
  -> AnyCellState
  -> CellState
  -> CellHTML
cellSourceRender def hided afterContent component initialState cs =
  if hided cs
  then H.text ""
  else shown
  where
  collapsedClass = if cs.isCollapsed then [CSS.collapsed] else []
  shown :: CellHTML
  shown =
    H.div [ P.classes $ join [ containerClasses, collapsedClass ] ]
    $ [ header def cs
      , row' (fadeWhen cs.isCollapsed)
        [ H.slot unit \_ -> { component: component, initialState: initialState } ]
      ]
      <> afterContent cs




-- | Constructs a cell component for an results-style cell.
makeResultsCellComponent
  :: forall s f
   . ResultsCellDef s f
  -> CellComponent
makeResultsCellComponent def = makeCellComponentPart def render
  where
  render
    :: Component AnyCellState InnerCellQuery Slam
    -> AnyCellState
    -> CellState
    -> CellHTML
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
                [ H.slot unit \_ -> { component: component
                                    , initialState: initialState
                                    }
                ]
            ]
        ]

  resLabel :: Maybe Port -> String
  resLabel p =
    maybe "" (\p -> Path.runFileName (Path.fileName p) ++ " :=")
    $ preview (_Resource <<< _filePath) =<< p

  nextCellButtons :: Maybe Port -> Array (CellHTML)
  nextCellButtons Nothing = []
  nextCellButtons (Just p) = case p of
    VarMap _ ->
      [ nextCellButton (Ace SQLMode) ]
    TaggedResource _ ->
      [ nextCellButton (Ace SQLMode)
      , nextCellButton Search
      , nextCellButton Viz
      , nextCellButton Download
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
  -> (Component AnyCellState InnerCellQuery Slam
      -> AnyCellState -> CellState -> CellHTML)
  -> CellComponent
makeCellComponentPart def render =
  parentComponent' (render component initialState) eval peek
  where

  _State :: PrismP AnyCellState s
  _State = clonePrism def._State

  _Query :: forall a. PrismP (InnerCellQuery a) (f a)
  _Query = clonePrism def._Query

  component :: Component AnyCellState InnerCellQuery Slam
  component =
    transform
    (review _State) (preview _State)
    (review _Query) (preview _Query)
    def.component

  initialState :: AnyCellState
  initialState = review _State def.initialState

  eval :: Natural CellQuery CellDSL
  eval (RunCell next) = pure next
  eval (StopCell next) = stopRun $> next
  eval (UpdateCell input k) = do
    liftAff =<< gets (^. _tickStopper)
    tickStopper <- startInterval
    modify (_tickStopper .~ tickStopper)
    cachingEnabled <- gets _.cachingEnabled
    let input' = prepareCellEvalInput cachingEnabled input
    modify (_input .~ input'.inputPort)
    result <- query unit (left (request (EvalCell input')))
    F.for_ result \{ output } -> modify (_hasResults .~ isJust output)
    liftAff tickStopper
    modify
      $ (_runState %~ finishRun)
      <<< (_output .~ (_.output =<< result))
      <<< (_messages .~ (maybe [] _.messages result))
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
  eval (SaveCell cellId cellType k) = do
    hasRun <- gets _.hasResults
    json <- query unit (left (request Save))
    pure (k { cellId, cellType, hasRun, state: fromMaybe jsonNull json })
  eval (LoadCell model next) =
    query unit (left (action (Load model.state))) $> next

  peek :: forall a. ChildF Unit InnerCellQuery a -> CellDSL Unit
  peek (ChildF _ q) = coproduct cellEvalPeek (const $ pure unit) q

  cellEvalPeek :: forall a. CellEvalQuery a -> CellDSL Unit
  cellEvalPeek (SetCanceler canceler _) = modify $ _canceler .~ canceler
  cellEvalPeek (SetupCell _ _) = modify $ _canceler .~ mempty
  cellEvalPeek (EvalCell _ _) = modify $ _canceler .~ mempty
  cellEvalPeek _ = pure unit

  stopRun :: CellDSL Unit
  stopRun = do
    cs <- gets _.canceler
    ts <- gets _.tickStopper
    liftAff ts
    liftAff $ cancel cs (Exn.error "Canceled")
    modify $ _runState .~ RunInitial

-- | Starts a timer running on an interval that passes Tick queries back to the
-- | component, allowing the runState to be updated with a timer.
-- |
-- | The returned value is an action that will stop the timer running when
-- | processed.
startInterval :: CellDSL (Slam Unit)
startInterval = do
  ref <- liftEff (newRef Nothing)
  start <- liftEff Date.now
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
