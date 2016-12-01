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

module SlamData.Workspace.Component.State
  ( State
  , initialState
  , _accessType
  , _loaded
  , _version
  , _initialDeckId
  , _stateMode
  , _cardGuideStep
  , _flipGuideStep
  , _lastVarMaps
  , _dirtyVarMaps
  , cardGuideStepNext
  , flipGuideStepNext
  ) where

import SlamData.Prelude

import Data.Lens (Lens', lens)
import Data.Map as Map

import SlamData.Workspace.AccessType (AccessType(..))
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.StateMode (StateMode(..))

type State =
  { accessType ∷ AccessType
  , loaded ∷ Boolean
  , version ∷ Maybe String
  , initialDeckId ∷ Maybe DeckId
  , stateMode ∷ StateMode
  , cardGuideStep ∷ Maybe Int
  , flipGuideStep ∷ Maybe Int
  , lastVarMaps ∷ Map.Map DeckId Port.URLVarMap
  , dirtyVarMaps ∷ Boolean
  }

initialState ∷ Maybe String → State
initialState version =
  { accessType: Editable
  , loaded: false
  , version
  , initialDeckId: Nothing
  , stateMode: Loading
  , cardGuideStep: Nothing
  , flipGuideStep: Nothing
  , lastVarMaps: Map.empty
  , dirtyVarMaps: false
  }

_accessType ∷ ∀ a r. Lens' {accessType ∷ a|r} a
_accessType = lens _.accessType _{accessType = _}

_loaded ∷ ∀ a r. Lens' {loaded ∷ a|r} a
_loaded = lens _.loaded _{loaded = _}

_version ∷ ∀ a r. Lens' {version ∷ a|r} a
_version = lens _.version _{version = _}

-- | This is only used while the workspace and initial deck are created, after
-- | that the value is irrelevant.
_initialDeckId ∷ ∀ a r. Lens' {initialDeckId ∷ a|r} a
_initialDeckId = lens _.initialDeckId _{initialDeckId = _}

_stateMode ∷ Lens' State StateMode
_stateMode = lens _.stateMode _{stateMode = _}

_cardGuideStep ∷ Lens' State (Maybe Int)
_cardGuideStep = lens _.cardGuideStep _{cardGuideStep = _}

_flipGuideStep ∷ Lens' State (Maybe Int)
_flipGuideStep = lens _.flipGuideStep _{flipGuideStep = _}

_lastVarMaps ∷ Lens' State (Map.Map DeckId Port.URLVarMap)
_lastVarMaps = lens _.lastVarMaps _{lastVarMaps = _}

_dirtyVarMaps ∷ Lens' State Boolean
_dirtyVarMaps = lens _.dirtyVarMaps _{dirtyVarMaps = _}

cardGuideStepNext ∷ State → State
cardGuideStepNext st = st { cardGuideStep = add 1 <$> st.cardGuideStep }

flipGuideStepNext ∷ State → State
flipGuideStepNext st = st { flipGuideStep = add 1 <$> st.flipGuideStep }
