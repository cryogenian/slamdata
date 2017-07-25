module SlamData.Workspace.Card.Setups.Viz.Eval.Common where

import SlamData.Prelude

import SlamData.Workspace.Card.Error as CE
import Control.Monad.State (class MonadState)
import Control.Monad.Writer.Class (class MonadTell)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Quasar.Class (class QuasarDSL)

type VizEval m a =
  MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ a
