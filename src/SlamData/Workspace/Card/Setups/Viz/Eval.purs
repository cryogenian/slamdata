module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Viz.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Array as A
import Data.StrMap as SM
import Data.Map as M

import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Viz.Model (Model)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Package.Projection as PP

import Utils (nothing)

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
  for_ modelErrors $ CE.throw

  pure $ Port.portOut Port.Viz
  where
  dimMap ∷ T.DimensionMap
  dimMap = fromMaybe T.emptyDimMap $ M.lookup m.vizType m.dimMaps

  guardPrj ∷ T.Projection → Maybe Unit
  guardPrj = nothing ∘ T.getProjection dimMap

  composeErrors = A.catMaybes ⋙ \x → if A.null x then Nothing else Just $ A.intercalate ", " x

  modelErrors ∷ Maybe String
  modelErrors = case m.vizType of
    VT.Metric →
      guardPrj PP._value $> "Value axis is not selected"
    VT.PivotTable →
      Nothing
    VT.Input _ →
      guardPrj PP._formValue $> "Value axis is not selected"
    VT.Select _ →
      guardPrj PP._formValue $> "Value axis is not selected"
    VT.Geo gt → case gt of
      VT.GeoHeatmap →
        composeErrors
        [ guardPrj PP._lat $> "Latitude is not selected"
        , guardPrj PP._lng $> "Longitude is not selected"
        , guardPrj PP._intensity $> "Intensity is not selected"
        ]
      VT.GeoMarker →
        composeErrors
        [ guardPrj PP._lat $> "Latitude is not selected"
        , guardPrj PP._lng $> "Longitude is not selected"
        , guardPrj PP._intensity $> "Intensity is not selected"
        ]
    VT.Chart ct → case ct of
      VT.Pie →
        composeErrors
        [ guardPrj PP._category $> "Category axis is not selected"
        , guardPrj PP._value $> "Measure axis is not selected"
        ]
      VT.Line →
        composeErrors
        [ guardPrj PP._dimension $> "Dimension axis is not selected"
        , guardPrj PP._value $> "Measure axis is not selected"
        ]
      VT.Bar →
        composeErrors
        [ guardPrj PP._category $> "Category axis is not selected"
        , guardPrj PP._value $> "Measure axis is not selected"
        ]
      VT.Area →
        composeErrors
        [ guardPrj PP._dimension $> "Dimension axis is not selected"
        , guardPrj PP._value $> "Measure axis is not selected"
        ]
      VT.Scatter →
        composeErrors
        [ guardPrj PP._abscissa $> "X-Axis is not selected"
        , guardPrj PP._ordinate $> "Y-Axis is not selected"
        ]
      VT.Radar →
        composeErrors
        [ guardPrj PP._category $> "Category axis is not selected"
        , guardPrj PP._value $> "Measure axis is not selected"
        ]
      VT.Funnel →
        composeErrors
        [ guardPrj PP._category $> "Category axis is not selected"
        , guardPrj PP._value $> "Measure axis is not selected"
        ]
      VT.Graph →
        composeErrors
        [ guardPrj PP._source $> "Source axis is not selected"
        , guardPrj PP._target $> "Target axis is not selected"
        ]
      VT.Heatmap →
        composeErrors
        [ guardPrj PP._abscissa $> "X-Axis is not selected"
        , guardPrj PP._ordinate $> "Y-Axis is not selected"
        , guardPrj PP._value $> "Measure axis is not selected"
        ]
      VT.Sankey →
        composeErrors
        [ guardPrj PP._abscissa $> "X-Axis is not selected"
        , guardPrj PP._ordinate $> "Y-Axis is not selected"
        ]
      VT.Gauge →
        guardPrj PP._value $> "Value axis is not selected"
      VT.Boxplot →
        composeErrors
        [ guardPrj PP._dimension $> "Dimension axis is not selected"
        , guardPrj PP._value $> "Measure axis is not selected"
        ]
      VT.PunchCard →
        composeErrors
        [ guardPrj PP._abscissa $> "X-Axis is not selected"
        , guardPrj PP._ordinate $> "Y-Axis is not selected"
        ]
      VT.Candlestick →
        composeErrors
        [ guardPrj PP._dimension $> "Dimension axis is not selected"
        , guardPrj PP._open $> "Open position axis is not selected"
        , guardPrj PP._close $> "Close position axis is not selected"
        , guardPrj PP._high $> "High position axis is not selected"
        , guardPrj PP._low $> "Low position axis is not selected"
        ]
      VT.Parallel →
        guardPrj (PP._dimIx 1) $> "At least two axes must be selected for parallels chart"
