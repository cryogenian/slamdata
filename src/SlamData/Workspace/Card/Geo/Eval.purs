module SlamData.Workspace.Card.Geo.Eval
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.State (class MonadState, put, get)
import Data.Lens ((^.))
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Port as Port
import SlamData.Effects (SlamDataEffects)

eval
  ∷ ∀ m
  . MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadAff SlamDataEffects m
  ⇒ QuasarDSL m
  ⇒ Port.GeoChartPort
  → Port.Resource
  → m Port.Port
eval gcPort resource = do
  let path = resource ^. Port._filePath
  results ← CE.liftQ $ QQ.all path
  let build = \leaf → gcPort.build leaf results
  evalState ← get
  case evalState of
    Just (ES.Geo st) → do
      layers × controls ← case st.leaflet of
        Nothing → pure $ st.layers × st.controls
        Just l → liftAff $ build l
      put $ Just $ ES.Geo { build, layers, controls, leaflet: st.leaflet }
    _ → put $ Just $ ES.Geo { leaflet: Nothing, build, layers: [ ], controls: [ ] }
  pure $ Port.ResourceKey Port.defaultResourceVar
