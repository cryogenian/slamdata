module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)
import Data.List as L
import Data.ListMap as LM
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Viz.Model (Model)
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Viz.Error as VE
import SlamData.Workspace.Card.Setups.DimensionMap.Package as PS
import SlamData.Workspace.Card.Setups.Common.Eval as BCE

lm ∷ ∀ a. LM.Module VT.VizType a
lm = LM.openModule VT.eq_

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
  records × axes ← BCE.analyze resource =<< get
  put $ Just $ CEM.Analysis { resource, records, axes }

  unless (L.null missingProjections)
    $ VE.throw $ { missingProjections, vizType: m.vizType }

  CE.throw "Auxiliary model is not provided, please contact support"

  where
  missingProjections ∷ L.List P.Projection
  missingProjections = L.filter (not ∘ flip P.member dimMap) requiredProjections

  requiredProjections ∷ L.List P.Projection
  requiredProjections = foldMap _.requiredFields $ lm.lookup m.vizType PS.packages

  dimMap ∷ P.DimMap
  dimMap = fromMaybe P.empty $ lm.lookup m.vizType m.dimMaps
