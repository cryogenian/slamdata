module SlamData.Workspace.Card.Setups.FormInput.Date.Eval where

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Eval as TL
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Model (Model)
import SlamData.Workspace.Card.Port as Port

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.Resource
  → m Port.Port
eval = TL.eval _.date FIT.Date
