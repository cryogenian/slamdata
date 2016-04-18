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

module SlamData.Notebook.Card.Component.State
  ( CardState(..)
  , CardStateP
  , initialCardState
  , _accessType
  , _visibility
  , _runState
  , _tickStopper
  , _isCollapsed
  , _messages
  , _messageVisibility
  , _hasResults
  , _cachingEnabled
  , _input
  , _output
  , _canceler
  , AnyCardState
  , _AceState
  , _MarkdownState
  , _SearchState
  , _JTableState
  , _VizState
  , _ChartState
  , _DownloadState
  , _APIState
  , _APIResultsState
  , _NextState
  , _SaveState
  , _OpenResourceState
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Canceler)

import Data.Lens (LensP, lens, PrismP, TraversalP, prism', wander)
import Data.Visibility (Visibility(..))

import Halogen (ParentState)

import SlamData.Notebook.AccessType (AccessType(..))
import SlamData.Notebook.Card.Ace.Component.State as Ace
import SlamData.Notebook.Card.API.Component.State as API
import SlamData.Notebook.Card.APIResults.Component.State as APIResults
import SlamData.Notebook.Card.Chart.Component.State as Chart
import SlamData.Notebook.Card.Component.Query (CardQuery, InnerCardQuery)
import SlamData.Notebook.Card.Download.Component.State as Download
import SlamData.Notebook.Card.JTable.Component.State as JTable
import SlamData.Notebook.Card.Markdown.Component.State as Markdown
import SlamData.Notebook.Card.Port (Port)
import SlamData.Notebook.Card.RunState (RunState(..))
import SlamData.Notebook.Card.Search.Component.State as Search
import SlamData.Notebook.Card.Viz.Component.State as Viz
import SlamData.Notebook.Card.Next.Component.State as Next
import SlamData.Notebook.Card.Save.Component.State as Save
import SlamData.Notebook.Card.OpenResource.Component.State as Open
import SlamData.Effects (Slam, SlamDataEffects)


-- | The common state value for notebook cards.
-- |
-- | - `accessType` tracks whether the card is in an editable or read-only
-- |   notebook. In the case of read-only notebooks editor cards are hidden.
-- | - `visibility` is used to specify whether the card should be rendered at
-- |   all - used when embedding a single card in another page.
-- | - `runState` tracks whether the card has run yet, is running, or has
-- |   completed running.
-- | - `isCollapsed` tracks whether the card is expanded or collapsed. In the
-- |   case of editor cards this shows/hides the whole editor, in the case of
-- |   results cards this shows/hides the evaluation messages.
-- | - `messages` is the list of error and informational messages generated
-- |   during evaluation. `Left` values are errors, `Right` values are
-- |   informational.
-- | - `messageVisibility` determines whether the messages should be shown or
-- |   not.
-- | - `hasResults` tracks whether the card has been evaluated successfully and
-- |   produced a result.
type CardState =
  { accessType ∷ AccessType
  , visibility ∷ Visibility
  , runState ∷ RunState
  , tickStopper ∷ Slam Unit
  , isCollapsed ∷ Boolean
  , messages ∷ Array (Either String String)
  , messageVisibility ∷ Visibility
  , hasResults ∷ Boolean
  , cachingEnabled ∷ Maybe Boolean -- Nothing if the option isn't available
  , input ∷ Maybe Port
  , output ∷ Maybe Port
  , canceler ∷ Canceler SlamDataEffects
  }

type CardStateP = ParentState CardState AnyCardState CardQuery InnerCardQuery Slam Unit

-- | Creates an initial `CardState` value for an editor card.
initialCardState ∷ CardState
initialCardState =
  { accessType: Editable
  , visibility: Visible
  , runState: RunInitial
  , tickStopper: pure unit
  , isCollapsed: false
  , messages: []
  , messageVisibility: Invisible
  , hasResults: false
  , cachingEnabled: Nothing
  , input: Nothing
  , output: Nothing
  , canceler: mempty
  }

_accessType ∷ LensP CardState AccessType
_accessType = lens _.accessType (_ { accessType = _ })

_visibility ∷ LensP CardState Visibility
_visibility = lens _.visibility (_ { visibility = _ })

_runState ∷ LensP CardState RunState
_runState = lens _.runState (_ { runState = _ })

_tickStopper ∷ LensP CardState (Slam Unit)
_tickStopper = lens _.tickStopper (_ { tickStopper = _ })

_isCollapsed ∷ LensP CardState Boolean
_isCollapsed = lens _.isCollapsed (_ { isCollapsed = _ })

_messages ∷ LensP CardState (Array (Either String String))
_messages = lens _.messages (_ { messages = _ })

_messageVisibility ∷ LensP CardState Visibility
_messageVisibility = lens _.messageVisibility (_ { messageVisibility = _ })

_hasResults ∷ LensP CardState Boolean
_hasResults = lens _.hasResults (_ { hasResults = _ })

_cachingEnabled ∷ TraversalP CardState Boolean
_cachingEnabled =
  wander \f s →
    case s.cachingEnabled of
      Nothing → pure s
      Just b → f b <#> \b' → s { cachingEnabled = Just b' }

-- | The last input value passed into the card when requesting evaluation.
_input ∷ LensP CardState (Maybe Port)
_input = lens _.input (_ { input = _ })

-- | The last output value computed for the card. This may not be up to date
-- | with the exact state of the card, but is the most recent result from when
-- | the card was evaluated.
_output ∷ LensP CardState (Maybe Port)
_output = lens _.output (_ { output = _ })

_canceler ∷ LensP CardState (Canceler SlamDataEffects)
_canceler = lens _.canceler _{canceler = _}

data AnyCardState
  = AceState Ace.StateP
  | MarkdownState Markdown.StateP
  | SearchState Search.State
  | JTableState JTable.State
  | VizState Viz.StateP
  | ChartState Chart.StateP
  | DownloadState Download.State
  | APIState API.StateP
  | APIResultsState APIResults.State
  | NextState Next.State
  | SaveState Save.State
  | OpenResourceState Open.State

_AceState ∷ PrismP AnyCardState Ace.StateP
_AceState = prism' AceState \s → case s of
  AceState s' → Just s'
  _ → Nothing

_MarkdownState ∷ PrismP AnyCardState Markdown.StateP
_MarkdownState = prism' MarkdownState \s → case s of
  MarkdownState s' → Just s'
  _ → Nothing

_SearchState ∷ PrismP AnyCardState Search.State
_SearchState = prism' SearchState \s → case s of
  SearchState s' → Just s'
  _ → Nothing

_JTableState ∷ PrismP AnyCardState JTable.State
_JTableState = prism' JTableState \s → case s of
  JTableState s' → Just s'
  _ → Nothing

_VizState ∷ PrismP AnyCardState Viz.StateP
_VizState = prism' VizState \s → case s of
  VizState s' → Just s'
  _ → Nothing

_ChartState ∷ PrismP AnyCardState Chart.StateP
_ChartState = prism' ChartState \s → case s of
  ChartState s' → Just s'
  _ → Nothing

_DownloadState ∷ PrismP AnyCardState Download.State
_DownloadState = prism' DownloadState \s → case s of
  DownloadState s' → Just s'
  _ → Nothing

_APIState ∷ PrismP AnyCardState API.StateP
_APIState = prism' APIState \s → case s of
  APIState s' → Just s'
  _ → Nothing

_APIResultsState ∷ PrismP AnyCardState APIResults.State
_APIResultsState = prism' APIResultsState \s → case s of
  APIResultsState s' → Just s'
  _ → Nothing

_NextState ∷ PrismP AnyCardState Next.State
_NextState = prism' NextState \s → case s of
  NextState s' → Just s'
  _ → Nothing

_SaveState ∷ PrismP AnyCardState Save.State
_SaveState = prism' SaveState \s → case s of
  SaveState s' → Just s'
  _ → Nothing

_OpenResourceState ∷ PrismP AnyCardState Open.State
_OpenResourceState = prism' OpenResourceState \s → case s of
  OpenResourceState s' → Just s'
  _ → Nothing
