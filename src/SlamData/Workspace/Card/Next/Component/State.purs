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

module SlamData.Workspace.Card.Next.Component.State where

import Prelude

import Data.Maybe as M
import Data.Lens (LensP, lens)

import SlamData.Workspace.Card.CardType as CT

type State =
  { types :: Array CT.CardType
    -- This would be unnecessary after error card and autorun for every card
    -- be implemented
  , message :: M.Maybe String
  }

_types :: ∀ a r. LensP {types :: a |r} a
_types = lens _.types (_{types = _})

_message :: ∀ a r. LensP {message :: a|r} a
_message = lens _.message (_{message = _})

initialState :: State
initialState =
  { types:
      [ CT.Ace CT.SQLMode
      , CT.Ace CT.MarkdownMode
      , CT.OpenResource
      , CT.API
      ]
  , message: M.Nothing
  }
