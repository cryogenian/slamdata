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

module SlamData.Workspace.Card.Component.State
  ( CardState
  , Status(..)
  , initialState
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Bus (BusRW)

import SlamData.Workspace.Card.Component.Query (Input)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

data Status
  = Pending
  | Initialized
  | Active
  | Inactive

derive instance eqStatus ∷ Eq Status

type CardState =
  { status ∷ Status
  , bus ∷ Maybe (BusRW Card.EvalMessage)
  , levelOfDetails ∷ LevelOfDetails
  }

initialState ∷ Input → CardState
initialState input =
  { status: if input.active then Active else Pending
  , bus: Nothing
  , levelOfDetails: High
  }
