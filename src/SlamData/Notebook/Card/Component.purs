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

module SlamData.Notebook.Card.Component
  ( CardComponent
  , makeCardComponent
  , module SlamData.Notebook.Card.Component.Def
  , module SlamData.Notebook.Card.Component.Query
  , module SlamData.Notebook.Card.Component.State
  ) where

import SlamData.Prelude

import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling (producerToStallingProducer)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Free (liftF)
import Control.Monad.Aff (cancel)
import Control.Monad.Eff.Exception as Exn

import Data.Array as Arr
import Data.Argonaut (jsonNull)
import Data.Date as Date
import Data.Function (on)
import Data.Lens (PrismP, review, preview, clonePrism, (.~), (%~))
import Data.Visibility (Visibility(..), toggleVisibility)

import DOM.Timer (interval, clearInterval)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Query.HalogenF (HalogenFP(..))
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource (_filePath)
import SlamData.Notebook.AccessType (AccessType(..))
import SlamData.Notebook.Card.CardType (CardType, AceMode(..), cardGlyph, cardName, nextCardTypes)
import SlamData.Notebook.Card.Common.EvalQuery (prepareCardEvalInput)
import SlamData.Notebook.Card.Component.Def (CardDef, makeQueryPrism, makeQueryPrism')
import SlamData.Notebook.Card.Component.Query (CardEvalInputPre, CardQueryP, InnerCardQuery, AnyCardQuery(..), CardEvalQuery(..), CardQuery(..), _APIQuery, _APIResultsQuery, _AceQuery, _AnyCardQuery, _CardEvalQuery, _ChartQuery, _DownloadQuery, _ExploreQuery, _JTableQuery, _MarkdownQuery, _SearchQuery, _VizQuery, _NextQuery, _SaveQuery, _OpenResourceQuery)
import SlamData.Notebook.Card.Component.Render (CardHTML, header, statusBar)
import SlamData.Notebook.Card.Component.State (AnyCardState, CardState, CardStateP, _APIResultsState, _APIState, _AceState, _ChartState, _DownloadState, _ExploreState, _JTableState, _MarkdownState, _SearchState, _VizState, _NextState, _accessType, _cachingEnabled, _canceler, _hasResults, _input, _isCollapsed, _messageVisibility, _messages, _output, _runState, _tickStopper, _visibility, initialCardState, _SaveState, _OpenResourceState)
import SlamData.Notebook.Card.Port (Port(..), _Resource, _Blocked)
import SlamData.Notebook.Card.RunState (RunState(..))
import SlamData.Render.Common (row', fadeWhen, glyph)
import SlamData.Render.CSS as CSS

-- | Type synonym for the full type of a card component.
type CardComponent = H.Component CardStateP CardQueryP Slam
type CardDSL = H.ParentDSL CardState AnyCardState CardQuery InnerCardQuery Slam Unit

-- | Card component factory
makeCardComponent
  ∷ ∀ s f
  . CardDef s f ()
  → CardComponent
makeCardComponent def = makeCardComponentPart def render
  where
  render
    ∷ H.Component AnyCardState InnerCardQuery Slam
    → AnyCardState
    → CardState
    → CardHTML
  render component initialState cs =
    if cs.visibility ≡ Invisible
      then HH.text ""
      else shown cs
    where
    canHaveOutput =
      not $ Arr.null $ nextCardTypes $ Just def.cardType
    shouldCollapse =
      cs.isCollapsed || isJust (cs.input >>= preview _Blocked)
    collapsedClass  =
      (guard shouldCollapse) $> CSS.collapsed

    hideIfCollapsed =
      ARIA.hidden $ show shouldCollapse

    shown ∷ CardState → CardHTML
    shown cs =
      HH.div [ HP.classes $ join [ containerClasses, collapsedClass  ] ]
      $ fold
          [
            header def.cardType cs
          , [ HH.div [ HP.classes ([B.row] ⊕ (fadeWhen shouldCollapse))
                     , hideIfCollapsed
                     ]
              [ HH.slot unit \_ → {component, initialState} ]
            ]
          , (guard canHaveOutput) $> statusBar cs.hasResults cs
          ]
containerClasses ∷ Array (HH.ClassName)
containerClasses = [B.containerFluid, CSS.notebookCard, B.clearfix]

-- | Constructs a card component from a record with the necessary properties and
-- | a render function.
makeCardComponentPart
  ∷ ∀ s f r
  . CardDef s f r
  → (H.Component AnyCardState InnerCardQuery Slam
     → AnyCardState
     → CardState
     → CardHTML
     )
  → CardComponent
makeCardComponentPart def render =
  H.parentComponent
    { render: render component initialState
    , eval
    , peek: Just (peek ∘ H.runChildF)
    }
  where

  _State ∷ PrismP AnyCardState s
  _State = clonePrism def._State

  _Query ∷ ∀ a. PrismP (InnerCardQuery a) (f a)
  _Query = clonePrism def._Query

  component ∷ H.Component AnyCardState InnerCardQuery Slam
  component =
    H.transform
      (review _State) (preview _State)
      (review _Query) (preview _Query)
      def.component

  initialState ∷ AnyCardState
  initialState = review _State def.initialState

  eval ∷ Natural CardQuery CardDSL
  eval (RunCard next) = pure next
  eval (StopCard next) = stopRun $> next
  eval (UpdateCard input k) = do
    H.fromAff =<< H.gets _.tickStopper
    tickStopper ← startInterval
    H.modify (_tickStopper .~ tickStopper)
    cachingEnabled ← H.gets _.cachingEnabled
    let input' = prepareCardEvalInput cachingEnabled input
    H.modify (_input .~ input'.inputPort)
    result ← H.query unit (left (H.request (EvalCard input')))
    for_ result \{ output } → H.modify (_hasResults .~ isJust output)
    H.fromAff tickStopper
    H.modify
      $ (_runState %~ finishRun)
      ∘ (_output .~ (_.output =<< result))
      ∘ (_messages .~ (maybe [] _.messages result))
    maybe (liftF HaltHF) (pure ∘ k ∘ _.output) result
  eval (RefreshCard next) = pure next
  eval (TrashCard next) = pure next
  eval (ToggleCollapsed next) =
    H.modify (_isCollapsed %~ not) $> next
  eval (ToggleMessages next) =
    H.modify (_messageVisibility %~ toggleVisibility) $> next
  eval (ToggleCaching next) =
    H.modify (_cachingEnabled %~ not) $> next
  eval (ShareCard next) = pure next
  eval (Tick elapsed next) =
    H.modify (_runState .~ RunElapsed elapsed) $> next
  eval (GetOutput k) = k <$> H.gets (_.output)
  eval (SaveCard cardId cardType k) = do
    { hasResults, cachingEnabled } ← H.get
    json ← H.query unit (left (H.request Save))
    pure ∘ k $
      { cardId
      , cardType
      , cachingEnabled
      , hasRun: hasResults
      , state: fromMaybe jsonNull json
      }
  eval (LoadCard model next) = do
    for_ model.cachingEnabled \b →
      H.modify (_cachingEnabled .~ b)
    H.query unit (left (H.action (Load model.state)))
    pure next
  eval (SetCardAccessType at next) =
    H.modify (_accessType .~ at) $> next

  peek ∷ ∀ a. InnerCardQuery a → CardDSL Unit
  peek = coproduct cardEvalPeek (const $ pure unit)

  cardEvalPeek ∷ ∀ a. CardEvalQuery a → CardDSL Unit
  cardEvalPeek (SetCanceler canceler _) = H.modify $ _canceler .~ canceler
  cardEvalPeek (SetupCard _ _) = H.modify $ _canceler .~ mempty
  cardEvalPeek (EvalCard _ _) = H.modify $ _canceler .~ mempty
  cardEvalPeek (NotifyStopCard _) = stopRun
  cardEvalPeek _ = pure unit

  stopRun ∷ CardDSL Unit
  stopRun = do
    cs ← H.gets _.canceler
    ts ← H.gets _.tickStopper
    H.fromAff ts
    H.fromAff $ cancel cs (Exn.error "Canceled")
    H.modify $ _runState .~ RunInitial

-- | Starts a timer running on an interval that passes Tick queries back to the
-- | component, allowing the runState to be updated with a timer.
-- |
-- | The returned value is an action that will stop the timer running when
-- | processed.
startInterval ∷ CardDSL (Slam Unit)
startInterval = do
  ref ← H.fromEff (newRef Nothing)
  start ← H.fromEff Date.now
  H.modify (_runState .~ RunElapsed zero)

  H.subscribe'
    $ EventSource
    $ producerToStallingProducer
    $ produce \emit → do
        i ← interval 1000 $ emit ∘ Left ∘ H.action ∘ Tick =<< do
          now ← Date.now
          pure $ on (-) Date.toEpochMilliseconds now start
        writeRef ref (Just i)

  pure $ maybe (pure unit) (H.fromEff ∘ clearInterval) =<< H.fromEff (readRef ref)

-- | Update the `RunState` from its current value to `RunFinished`.
finishRun ∷ RunState → RunState
finishRun RunInitial = RunElapsed zero
finishRun (RunElapsed ms) = RunFinished ms
finishRun (RunFinished ms) = RunFinished ms
