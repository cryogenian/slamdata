module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Viz.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)

import Data.StrMap as SM
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
import SlamData.Workspace.Card.Setups.Package.Projection as PP

import Utils (nothing)

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
  unless (L.null missingAxes) $ VE.throw $ VE.MissingAxesError missingAxes

  pure $ Port.portOut Port.Viz
  where
  dimMap ∷ T.DimensionMap
  dimMap = fromMaybe T.emptyDimMap $ M.lookup m.vizType m.dimMaps

  guardPrj ∷ T.Projection → L.List T.Projection
  guardPrj prj =
    if T.hasProjection dimMap prj then L.Nil else L.singleton prj

  catValue ∷ L.List T.Projection
  catValue = foldMap guardPrj [ PP._category, PP._value ]

  dimValue ∷ L.List T.Projection
  dimValue = foldMap guardPrj [ PP._dimension, PP._value ]

  sourceTarget ∷ L.List T.Projection
  sourceTarget = foldMap guardPrj [ PP._source, PP._target ]

  abscissaOrdinate ∷ L.List T.Projection
  abscissaOrdinate = foldMap guardPrj [ PP._abscissa, PP._ordinate ]

  latLng ∷ L.List T.Projection
  latLng = foldMap guardPrj [ PP._lat, PP._lng ]

  missingAxes ∷ L.List T.Projection
  missingAxes = case m.vizType of
    VT.Metric →
      guardPrj PP._value
    VT.PivotTable →
      L.Nil
    VT.Input _ →
      guardPrj PP._formValue
    VT.Select _ →
      guardPrj PP._formValue
    VT.Geo gt → case gt of
      VT.GeoHeatmap →
        latLng <> guardPrj PP._intensity
      VT.GeoMarker →
        latLng
    VT.Chart ct → case ct of
      VT.Pie →
        catValue
      VT.Line →
        dimValue
      VT.Bar →
        catValue
      VT.Area →
        dimValue
      VT.Scatter →
        abscissaOrdinate
      VT.Radar →
        catValue
      VT.Funnel →
        catValue
      VT.Graph →
        sourceTarget
      VT.Heatmap →
        guardPrj PP._value <> abscissaOrdinate
      VT.Sankey →
        sourceTarget
      VT.Gauge →
        guardPrj PP._value
      VT.Boxplot →
        dimValue
      VT.PunchCard →
        abscissaOrdinate
      VT.Candlestick →
        foldMap guardPrj [ PP._dimension, PP._open, PP._close, PP._high, PP._low ]
      VT.Parallel →
        guardPrj $ PP._dimIx 1
