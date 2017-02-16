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

module SlamData.Workspace.Card.Component.Query
  ( CardQuery(..)
  , Input
  , InnerCardQuery
  , module EQ
  ) where

import SlamData.Prelude

import SlamData.Workspace.Card.Common.EvalQuery as EQ
import SlamData.Workspace.Eval.Card as Card

type Input =
  { active ∷ Boolean
  }

data CardQuery a
  = Initialize a
  | UpdateDimensions a
  | PreloadCard a
  | HandleInput Input a
  | HandleEvalMessage (Card.EvalMessage) a
  | HandleCardMessage (EQ.CardEvalMessage) a
  | ZoomIn a

type InnerCardQuery f = Coproduct EQ.CardEvalQuery f
