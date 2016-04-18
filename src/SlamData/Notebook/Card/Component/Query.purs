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

module SlamData.Notebook.Card.Component.Query
  ( CardQuery(..)
  , CardQueryP
  , InnerCardQuery
  , _CardEvalQuery
  , _AnyCardQuery
  , AnyCardQuery(..)
  , _AceQuery
  , _MarkdownQuery
  , _SearchQuery
  , _JTableQuery
  , _VizQuery
  , _ChartQuery
  , _DownloadQuery
  , _APIQuery
  , _APIResultsQuery
  , _NextQuery
  , _SaveQuery
  , _OpenResourceQuery
  , module SlamData.Notebook.Card.Common.EvalQuery
  ) where

import SlamData.Prelude

import Data.Lens (PrismP, prism')
import Data.Lens.Prism.Coproduct (_Left, _Right)
import Data.Time (Milliseconds)

import Halogen (ChildF)

import SlamData.Notebook.AccessType as Na

import SlamData.Notebook.Card.Ace.Component.Query as Ace
import SlamData.Notebook.Card.API.Component.Query as API
import SlamData.Notebook.Card.APIResults.Component.Query as APIResults
import SlamData.Notebook.Card.CardId (CardId)
import SlamData.Notebook.Card.CardType (CardType)
import SlamData.Notebook.Card.Chart.Component.Query as Chart
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery(..), CardEvalInputPre)
import SlamData.Notebook.Card.Download.Component.Query as Download
import SlamData.Notebook.Card.JTable.Component.Query as JTable
import SlamData.Notebook.Card.Markdown.Component.Query as Markdown
import SlamData.Notebook.Card.OpenResource.Component.Query as Open
import SlamData.Notebook.Card.Model as Card
import SlamData.Notebook.Card.Port (Port)
import SlamData.Notebook.Card.Search.Component.Query as Search
import SlamData.Notebook.Card.Viz.Component.Query as Viz
import SlamData.Notebook.Card.Next.Component.Query as Next
import SlamData.Notebook.Card.Save.Component.Query as Save

-- | The common query algebra for a notebook card.
-- |
-- | - `RunCard` is captured by the notebook and used to call `UpdateCard` on
-- |   the card that raised it, passing in an input value if one is required.
-- | - `UpdateCard` accepts an input value from a parent card if one is
-- |   required, performs any necessary actions to evalute the card and update
-- |   its state, and then returns its own output value.
-- | - `RefreshCard` is captured by the notebook and goes to the root of the
-- |   current card's dependencies and updates the cards downwards from there.
-- | - `TrashCard` is captured by the notebook, removes the current card and
-- |   any dependencies.
-- | - `ToggleCollapsed` is used to toggle the visibility of the editor
-- |   part of the card.
-- | - `ToggleMessages` is used to toggle the visibility of the status/error
-- |   messages generated while evaluating the card.
-- | - `ToggleCaching` is used to toggle the use of the views & query APIs.
-- | - `ShareCard` is captured by the notebook and should raise a dialog with a
-- |   share/embed message appropriate for the card.
data CardQuery a
  = RunCard a
  | StopCard a
  | UpdateCard CardEvalInputPre (Maybe Port → a)
  | RefreshCard a
  | TrashCard a
  | ToggleCollapsed a
  | ToggleMessages a
  | ToggleCaching a
  | ShareCard a
  | Tick Milliseconds a
  | GetOutput (Maybe Port → a)
  | SaveCard CardId CardType (Card.Model → a)
  | LoadCard Card.Model a
  | SetCardAccessType Na.AccessType a

type CardQueryP = Coproduct CardQuery (ChildF Unit InnerCardQuery)

type InnerCardQuery = Coproduct CardEvalQuery AnyCardQuery

_CardEvalQuery ∷ ∀ a. PrismP (InnerCardQuery a) (CardEvalQuery a)
_CardEvalQuery = _Left

_AnyCardQuery ∷ ∀ a. PrismP (InnerCardQuery a) (AnyCardQuery a)
_AnyCardQuery = _Right

data AnyCardQuery a
  = AceQuery (Ace.QueryP a)
  | MarkdownQuery (Markdown.QueryP a)
  | SearchQuery (Search.Query a)
  | JTableQuery (JTable.QueryP a)
  | VizQuery (Viz.QueryP a)
  | ChartQuery (Chart.QueryP a)
  | DownloadQuery (Download.QueryP a)
  | APIQuery (API.QueryP a)
  | APIResultsQuery (APIResults.QueryP a)
  | NextQuery (Next.QueryP a)
  | SaveQuery (Save.QueryP a)
  | OpenResourceQuery (Open.QueryP a)

_AceQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Ace.QueryP a)
_AceQuery = prism' AceQuery \q → case q of
  AceQuery q' → Just q'
  _ → Nothing

_MarkdownQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Markdown.QueryP a)
_MarkdownQuery = prism' MarkdownQuery \q → case q of
  MarkdownQuery q' → Just q'
  _ → Nothing

_SearchQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Search.Query a)
_SearchQuery = prism' SearchQuery \q → case q of
  SearchQuery q' → Just q'
  _ → Nothing

_JTableQuery ∷ ∀ a. PrismP (AnyCardQuery a) (JTable.QueryP a)
_JTableQuery = prism' JTableQuery \q → case q of
  JTableQuery q' → Just q'
  _ → Nothing

_VizQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Viz.QueryP a)
_VizQuery = prism' VizQuery \q → case q of
  VizQuery q' → Just q'
  _ → Nothing

_ChartQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Chart.QueryP a)
_ChartQuery = prism' ChartQuery \q → case q of
  ChartQuery q' → Just q'
  _ → Nothing

_DownloadQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Download.QueryP a)
_DownloadQuery = prism' DownloadQuery \q → case q of
  DownloadQuery q' → Just q'
  _ → Nothing

_APIQuery ∷ ∀ a. PrismP (AnyCardQuery a) (API.QueryP a)
_APIQuery = prism' APIQuery \q → case q of
  APIQuery q' → Just q'
  _ → Nothing

_APIResultsQuery ∷ ∀ a. PrismP (AnyCardQuery a) (APIResults.QueryP a)
_APIResultsQuery = prism' APIResultsQuery \q → case q of
  APIResultsQuery q' → Just q'
  _ → Nothing

_NextQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Next.QueryP a)
_NextQuery = prism' NextQuery \q → case q of
  NextQuery q' → Just q'
  _ → Nothing

_SaveQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Save.QueryP a)
_SaveQuery = prism' SaveQuery \q → case q of
  SaveQuery q' → Just q'
  _ → Nothing

_OpenResourceQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Open.QueryP a)
_OpenResourceQuery = prism' OpenResourceQuery \q → case q of
  OpenResourceQuery q' → Just q'
  _ → Nothing
