module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Viz.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Map as M
import Data.List as L

import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Viz.Model (Model)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Setups.Viz.Error as VE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.DimMap.Component.State as DS

eval
  ∷ ∀ m v
  . MonadState CEM.CardState m
  ⇒ MonadThrow (Variant (qerror ∷ CE.QError, setupViz ∷ VE.Error | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ Model
  → Port.Resource
  → m Port.Out
eval m resource = do
  records × axes ← BCE.analyze resource =<< get
  put $ Just $ CEM.Analysis { resource, records, axes }
  unless (L.null missingProjections) $ VE.throw $ { missingProjections, vizType }

  pure $ Port.portOut Port.Viz
  where
  vizType ∷ VT.VizType
  vizType = m.vizType

  requiredProjections ∷ L.List T.Projection
  requiredProjections = foldMap _.requiredFields $ M.lookup vizType DS.packages

  missingProjections ∷ L.List T.Projection
  missingProjections = L.filter (not ∘ T.hasProjection dimMap) requiredProjections

  dimMap ∷ T.DimensionMap
  dimMap = fromMaybe T.emptyDimMap $ M.lookup m.vizType m.dimMaps
