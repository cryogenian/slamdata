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

module SlamData.Notebook.Component.State where

import SlamData.Prelude

import Data.BrowserFeatures (BrowserFeatures)
import Data.Lens (LensP, lens)
import Data.Shortcut as Shortcut
import Data.StrMap (StrMap, fromFoldable)

import DOM.Event.EventTarget (EventListener)

import SlamData.Notebook.AccessType (AccessType(..))
import SlamData.Notebook.Card.CardId (CardId)
import SlamData.Notebook.Card.CardType (CardType(..), AceMode(..))
import SlamData.Notebook.Deck.Component.Query (Query(..))
import SlamData.Effects (SlamDataEffects)
import SlamData.Notebook.Menu.Component.Query (Value, deckQueryToValue)

type NotebookShortcut =
  { shortcut ∷ Shortcut.Shortcut, value ∷ Value, label ∷ Maybe String }

type State =
  { accessType ∷ AccessType
  , browserFeatures ∷ BrowserFeatures
  , notebookShortcuts ∷ StrMap NotebookShortcut
  , keyboardListeners ∷ Array (EventListener SlamDataEffects)
  , loaded ∷ Boolean
  , viewingCard ∷ Maybe CardId
  , version ∷ Maybe String
  , parentHref ∷ Maybe String
  }

notebookShortcuts ∷ StrMap NotebookShortcut
notebookShortcuts =
  fromFoldable
    [ Tuple
        "NotebookPublish"
        { shortcut: Shortcut.modP
        , value: deckQueryToValue $ Publish unit
        , label: Nothing
        }
    , Tuple
        "InsertQuery"
        { shortcut: Shortcut.altModOne
        , value: deckQueryToValue $ AddCard (Ace SQLMode) unit
        , label: Nothing
        }
    , Tuple
        "InsertMarkdown"
        { shortcut: Shortcut.altModTwo
        , value: deckQueryToValue $ AddCard (Ace MarkdownMode) unit
        , label: Nothing
        }
    , Tuple
        "InsertExplore"
        { shortcut: Shortcut.altModThree
        , value: deckQueryToValue $ AddCard OpenResource unit
        , label: Nothing
        }
    , Tuple
        "InsertSearch"
        { shortcut: Shortcut.altModFour
        , value: deckQueryToValue $ AddCard Search unit
        , label: Nothing
        }
    , Tuple
        "InsertAPI"
        { shortcut: Shortcut.altModFive
        , value: deckQueryToValue $ AddCard API unit
        , label: Nothing
        }
    ]

initialState ∷ { browserFeatures ∷ BrowserFeatures } → State
initialState r =
  { accessType: Editable
  , browserFeatures: r.browserFeatures
  , notebookShortcuts: notebookShortcuts
  , keyboardListeners: []
  , loaded: false
  , viewingCard: Nothing
  , version: Nothing
  , parentHref: Nothing
  }

_accessType ∷ ∀ a r. LensP {accessType ∷ a|r} a
_accessType = lens _.accessType _{accessType = _}

_browserFeatures ∷ ∀ a r. LensP {browserFeatures ∷ a|r} a
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

_notebookShortcuts ∷ ∀ a r. LensP {notebookShortcuts ∷ a |r} a
_notebookShortcuts = lens _.notebookShortcuts _{notebookShortcuts = _}

_keyboardListeners ∷ ∀ a r. LensP {keyboardListeners ∷ a |r} a
_keyboardListeners = lens _.keyboardListeners _{keyboardListeners = _}

_loaded ∷ ∀ a r. LensP {loaded ∷ a|r} a
_loaded = lens _.loaded _{loaded = _}

_viewingCard ∷ ∀ a r. LensP {viewingCard ∷ a|r} a
_viewingCard = lens _.viewingCard _{viewingCard = _}

_version ∷ ∀ a r. LensP {version ∷ a|r} a
_version = lens _.version _{version = _}

_parentHref ∷ ∀ a r. LensP {parentHref ∷ a |r} a
_parentHref = lens _.parentHref _{parentHref = _}
