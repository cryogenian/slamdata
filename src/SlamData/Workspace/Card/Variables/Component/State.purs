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

module SlamData.Workspace.Card.Variables.Component.State
  ( State
  , initialState
  , StateP
  ) where

import SlamData.Prelude

import Halogen (ParentState)

import SlamData.Workspace.Card.Common.EvalQuery as NC
import SlamData.Effects (Slam)
import SlamData.Workspace.FormBuilder.Component as FB

type State = {}

initialState :: State
initialState = {}

type StateP =
  ParentState
    State
    (FB.StateP Slam)
    NC.CardEvalQuery
    FB.QueryP
    Slam
    Unit
