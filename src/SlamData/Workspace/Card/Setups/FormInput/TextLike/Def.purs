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
