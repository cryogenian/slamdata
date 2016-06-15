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

module SlamData.Workspace.Card.Markdown.Component.State
  ( StateP
  , module Core
  ) where

import SlamData.Prelude

import Halogen (ParentState)

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.Markdown.Component.Query (Query)
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Markdown.Component.State.Core as Core
import Text.Markdown.SlamDown.Halogen.Component as SDH

type StateP =
  ParentState
    Core.State
    (SDH.SlamDownState VM.VarMapValue)
    (CardEvalQuery ⨁ Query)
    (SDH.SlamDownQuery VM.VarMapValue)
    Slam
    Unit
