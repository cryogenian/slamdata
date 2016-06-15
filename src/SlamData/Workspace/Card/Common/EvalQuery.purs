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

module SlamData.Workspace.Card.Common.EvalQuery
  ( CardEvalQuery(..)
  , module SlamData.Workspace.Card.Eval.CardEvalT
  ) where

import SlamData.Prelude

import SlamData.Workspace.Card.Eval.CardEvalT (CardEvalInput, CardEvalT, runCardEvalT, runCardEvalT_, temporaryOutputResource)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Model (AnyCardModel)

-- | The query algebra shared by the inner parts of a card component.
-- |
-- | - `EvalCard` is a command sent from the deck when a card component needs
-- |   updating during the evaluation process. An input value from the previous
-- |   card in the deck is provided, along with the output from the card's model
-- |   evaluator. The card cannot return a new port value, it's eval is only
-- |   allowed to update the card component state.
data CardEvalQuery a
  = EvalCard CardEvalInput (Maybe Port.Port) a
  | Save (AnyCardModel → a)
  | Load AnyCardModel a
  | SetDimensions { width ∷ Number, height ∷ Number } a
