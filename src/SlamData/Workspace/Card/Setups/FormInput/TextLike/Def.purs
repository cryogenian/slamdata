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

module SlamData.Workspace.Card.Setups.FormInput.TextLike.Def where

import SlamData.Prelude

import Data.Lens (APrism')
import Data.Argonaut (JCursor)

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.Query as Q
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.State as ST
import SlamData.Workspace.Card.Setups.Axis as Ax

type TextLikeDef =
  { _State ∷ APrism' CCS.AnyCardState ST.StateP
  , _Query ∷ ∀ a. APrism' (Coproduct CardEvalQuery CCQ.AnyCardQuery a) (Q.QueryP a)
  , valueProjection ∷ Ax.Axes → Array JCursor
  }
rray JCursor
  }
