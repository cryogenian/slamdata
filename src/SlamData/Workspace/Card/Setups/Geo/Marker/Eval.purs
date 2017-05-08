module SlamData.Workspace.Card.Setups.Geo.Marker.Eval
  ( eval
  , module M
  ) where

import SlamData.Prelude

import Control.Monad.Writer.Class (class MonadTell)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Geo.Marker.Model as M

eval
  ∷ ∀ m
  . MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ M.Model
  → Port.DataMap
  → m Port.Out
eval model varMap =
  pure $ Port.portOut Port.Terminal
