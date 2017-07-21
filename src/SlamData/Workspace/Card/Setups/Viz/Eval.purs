module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Writer.Class (class MonadTell)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Viz.Model (Model)

type VizEval m v =
  MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ Model
  → Port.Resource
  → m Port.Out

eval ∷ ∀ m v. VizEval m v
eval m resource = do
  CE.throw "Auxiliary model is not provided, please contact support"
