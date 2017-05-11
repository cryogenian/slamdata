module SlamData.Workspace.Card.Geo.Eval
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, modify, get)
import Data.Argonaut (Json)
import Data.Lens ((^.))
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Port as Port

eval
  ∷ ∀ m
  . MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ QuasarDSL m
  ⇒ Port.GeoChartPort
  → Port.Resource
  → m Port.Port
eval gcPort resource = do
  let path = resource ^. Port._filePath
  results ← CE.liftQ $ QQ.all path
  let build = \leaf → gcPort.build leaf results
  modify case _ of
    Just (ES.Geo st) → Just $ ES.Geo $ st
      { build = build
      , layers = case st.leaflet of
          Nothing → st.layers
          Just l → build l
      }
    _ → Just $ ES.Geo { leaflet: Nothing, build, layers: [ ] }
  get >>= traceAnyA
--  case gcPort.gcType of
--    Heatmap → evalHeatmap results
--    Marker → evalMarker results
--  put $ Just $ ES.ChartOptions (buildOptions results)
  pure $ Port.ResourceKey Port.defaultResourceVar
