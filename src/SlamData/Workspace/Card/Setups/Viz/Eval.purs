{-
Copyright 2017 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.State (get, put)
import Data.Argonaut as J
import Data.Array as A
import Data.Lens ((^?))
import Data.List as L
import Data.ListMap as LM
import Data.StrMap as SM
import Data.Variant (on)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Setups.Viz.Model (Model)
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Viz.Error as VE
import SlamData.Workspace.Card.Setups.DimensionMap.Package as PS
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Select as Select
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Viz.Eval.Area as Area
import SlamData.Workspace.Card.Setups.Viz.Eval.Bar as Bar
import SlamData.Workspace.Card.Setups.Viz.Eval.Boxplot as Boxplot
import SlamData.Workspace.Card.Setups.Viz.Eval.Candlestick as Candlestick
import SlamData.Workspace.Card.Setups.Viz.Eval.Funnel as Funnel
import SlamData.Workspace.Card.Setups.Viz.Eval.Gauge as Gauge
import SlamData.Workspace.Card.Setups.Viz.Eval.GeoMarker as GeoMarker
import SlamData.Workspace.Card.Setups.Viz.Eval.GeoHeatmap as GeoHeatmap
import SlamData.Workspace.Card.Setups.Viz.Eval.Graph as Graph
import SlamData.Workspace.Card.Setups.Viz.Eval.Heatmap as Heatmap
import SlamData.Workspace.Card.Setups.Viz.Eval.Line as Line
import SlamData.Workspace.Card.Setups.Viz.Eval.Parallel as Parallel
import SlamData.Workspace.Card.Setups.Viz.Eval.Pie as Pie
import SlamData.Workspace.Card.Setups.Viz.Eval.PunchCard as PunchCard
import SlamData.Workspace.Card.Setups.Viz.Eval.Radar as Radar
import SlamData.Workspace.Card.Setups.Viz.Eval.Sankey as Sankey
import SlamData.Workspace.Card.Setups.Viz.Eval.Scatter as Scatter


import Unsafe.Coerce (unsafeCoerce)

hole ∷ ∀ a. a
hole = unsafeCoerce unit

lm ∷ ∀ a. LM.Module VT.VizType a
lm = LM.openModule VT.eq_


eval ∷ ∀ m. VizEval m (Model → Port.Resource → m Port.Out)
eval m resource = do
  records × axes ← BCE.analyze resource =<< get
  put $ Just $ CEM.Analysis { resource, records, axes }

  checkMissingProjections

  iOut ← for (CT.contractToInput m.vizType) evalInput
  sOut ← for (CT.contractToSelect m.vizType) $ evalSelect records
  gOut ← for (CT.contractToGeo m.vizType) evalGeo
  cOut ← for (CT.contractToChart m.vizType) $ evalChart records
  stOut ← for (CT.contractToStatic m.vizType) $ evalStatic records

  maybe throwUnexpected pure
    $ iOut <|> sOut <|> gOut <|> cOut <|> stOut

  where
  throwUnexpected ∷ ∀ a. m a
  throwUnexpected =
    CE.throw "Unexpected visualization type, please contact support"

  getDimMap ∷ m P.DimMap
  getDimMap = maybe
    ( CE.throw
       $ "Setup Visualization model has no field for "
       ⊕ VT.name m.vizType )
    pure
    $ lm.lookup m.vizType m.dimMaps

  getAux ∷ m Aux.State
  getAux = maybe
    ( CE.throw
        $ "Setup Visualization model has no auxiliary model for "
        ⊕ VT.name m.vizType
    )
    pure
    $ lm.lookup m.vizType m.auxes


  evalInput ∷ CT.Input () → m Port.Out
  evalInput formInputType = do
    dimMap ← getDimMap
    -- Already checked presented in DSL interpretation
    let
      projection = unsafePartial fromJust $ P.lookup P.formValue dimMap
      port = Port.SetupInput { projection, formInputType }
    pure $ port × SM.singleton Port.defaultResourceVar (Left resource)

  evalStatic ∷ Array J.Json → CT.Static () → m Port.Out
  evalStatic records _ =
    pure $ port × SM.singleton Port.defaultResourceVar (Left resource)
    where
    port = Port.CategoricalMetric { value, label: Nothing }
    value = fromMaybe "" do
      record ← A.head records
      dimMap ← lm.lookup m.vizType m.dimMaps
      dim ← P.lookup P.value dimMap
      cursor ← dim ^? D._value ∘ D._projection
      Sem.getMaybeString record cursor

  evalSelect ∷ Array J.Json → CT.Select () → m Port.Out
  evalSelect records formInputType = do
    dimMap ← getDimMap
    port ← Select.eval records formInputType $ unsafePartial fromJust $ P.lookup P.formValue dimMap
    pure $ port × SM.singleton Port.defaultResourceVar (Left resource)

  evalGeo ∷ CT.Geo () → m Port.Out
  evalGeo gct = do
    dimMap ← getDimMap
    aux ← getAux
    case_
      # on CT._geoMarker (const $ GeoMarker.eval dimMap aux resource)
      # on CT._geoHeatmap (const $ GeoHeatmap.eval dimMap aux resource)
      $ gct

  evalChart ∷ Array J.Json → CT.Chart () → m Port.Out
  evalChart records cht = do
    dimMap ← getDimMap
    aux ← getAux
    case_
      # on CT._area (const $ Area.eval dimMap aux resource)
      # on CT._bar (const $ Bar.eval dimMap aux resource)
      # on CT._boxplot (const $ Boxplot.eval dimMap aux resource)
      # on CT._candlestick (const $ Candlestick.eval dimMap aux resource)
      # on CT._funnel (const $ Funnel.eval dimMap aux resource)
      # on CT._gauge (const $ Gauge.eval dimMap aux resource)
      # on CT._graph (const $ Graph.eval dimMap aux resource)
      # on CT._heatmap (const $ Heatmap.eval dimMap aux resource)
      # on CT._line (const $ Line.eval dimMap aux resource)
      # on CT._metric (const $ evalMetric records)
      # on CT._parallel (const $ Parallel.eval dimMap aux resource)
      # on CT._pie (const $ Pie.eval dimMap aux resource)
      # on CT._punchCard (const $ PunchCard.eval dimMap aux resource)
      # on CT._radar (const $ Radar.eval dimMap aux resource)
      # on CT._sankey (const $ Sankey.eval dimMap aux resource)
      # on CT._scatter (const $ Scatter.eval dimMap aux resource)
      # on CT._pivot (const $ CE.throw "Pivot table case should be handled separately")
      $ cht

  evalMetric ∷ Array J.Json → m Port.Out
  evalMetric records = do
    pure $ port × SM.singleton Port.defaultResourceVar (Left resource)
    where
    port = Port.CategoricalMetric { value, label: Nothing }
    value = fromMaybe "" do
      record ← A.head records
      dimMap ← lm.lookup m.vizType m.dimMaps
      dim ← P.lookup P.value dimMap
      cursor ← dim ^? D._value ∘ D._projection
      Sem.getMaybeString record cursor


  checkMissingProjections ∷ m Unit
  checkMissingProjections = do
    dimMap ← getDimMap
    let missingProjections = L.filter (not ∘ flip P.member dimMap) requiredProjections
    unless (L.null missingProjections)
      $ VE.throw $ { missingProjections, vizType: m.vizType }

  requiredProjections ∷ L.List P.Projection
  requiredProjections = foldMap _.requiredFields $ lm.lookup m.vizType PS.packages
