module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Viz.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)

import Data.StrMap as SM

import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Viz.Model (Model)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Quasar.Class (class QuasarDSL)

eval
  ∷ ∀ m
  . MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ Model
  → Port.Resource
  → m Port.Out
eval m resource = do
  records × axes ← BCE.analyze resource =<< get
  put $ Just $ CEM.Analysis { resource, records, axes }
  pure $ Port.ResourceKey "foo" × SM.empty
