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

module SlamData.Notebook.Card.Markdown.Component.State where

import SlamData.Notebook.Card.Port.VarMap as VM

import SlamData.Prelude

import Halogen (ParentState)

import Text.Markdown.SlamDown (SlamDownP)
import Text.Markdown.SlamDown.Halogen.Component (SlamDownState, SlamDownQuery)

import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Effects (Slam)

type State = Maybe (SlamDownP VM.VarMapValue)

initialState :: State
initialState = Nothing

type StateP =
  ParentState
    State (SlamDownState VM.VarMapValue)
    CardEvalQuery (SlamDownQuery VM.VarMapValue)
    Slam Unit
