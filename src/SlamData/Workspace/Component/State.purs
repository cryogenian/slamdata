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
  , _path
  , _initialDeckId
  , _stateMode
  , _cardGuideStep
  , _flipGuideStep
  , cardGuideStepNext
  , flipGuideStepNext
  ) where

import SlamData.Prelude

import Data.Lens (LensP, lens)

import SlamData.Workspace.AccessType (AccessType(..))
import SlamData.Workspace.StateMode (StateMode(..))
import SlamData.Workspace.Deck.DeckId (DeckId)

import Utils.Path (DirPath)

type State =
  { accessType ∷ AccessType
  , loaded ∷ Boolean
  , version ∷ Maybe String
  , path ∷ Maybe DirPath
  , initialDeckId ∷ Maybe DeckId
  , stateMode ∷ StateMode
  , cardGuideStep ∷ Maybe Int
  , flipGuideStep ∷ Maybe Int
  }

initialState ∷ Maybe String → State
initialState version =
  { accessType: Editable
  , loaded: false
  , version
  , path: Nothing
  , initialDeckId: Nothing
  , stateMode: Loading
  , cardGuideStep: Nothing
  , flipGuideStep: Nothing
  }

_accessType ∷ ∀ a r. LensP {accessType ∷ a|r} a
_accessType = lens _.accessType _{accessType = _}

_loaded ∷ ∀ a r. LensP {loaded ∷ a|r} a
_loaded = lens _.loaded _{loaded = _}

_version ∷ ∀ a r. LensP {version ∷ a|r} a
_version = lens _.version _{version = _}

_path ∷ ∀ a r. LensP {path ∷ a|r} a
_path = lens _.path _{path = _}

-- | This is only used while the workspace and initial deck are created, after
-- | that the value is irrelevant.
_initialDeckId ∷ ∀ a r. LensP {initialDeckId ∷ a|r} a
_initialDeckId = lens _.initialDeckId _{initialDeckId = _}

_stateMode ∷ LensP State StateMode
_stateMode = lens _.stateMode _{stateMode = _}

_cardGuideStep ∷ LensP State (Maybe Int)
_cardGuideStep = lens _.cardGuideStep _{cardGuideStep = _}

_flipGuideStep ∷ LensP State (Maybe Int)
_flipGuideStep = lens _.flipGuideStep _{flipGuideStep = _}

cardGuideStepNext ∷ State → State
cardGuideStepNext st = st { cardGuideStep = add 1 <$> st.cardGuideStep }

flipGuideStepNext ∷ State → State
flipGuideStepNext st = st { flipGuideStep = add 1 <$> st.flipGuideStep }
