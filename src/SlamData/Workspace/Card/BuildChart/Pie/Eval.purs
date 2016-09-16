module SlamData.Workspace.Card.BuildChart.Pie.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Pie.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
import Data.Lens as Lens
import Data.Map as M
import Data.Int as Int

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Pie.Model (Model, PieR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Pie))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Chart.Semantics as Sem
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port


eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  numRecords ←
    CET.liftQ $ QQ.count resource

  when (numRecords > 10000)
    $ QE.throw
    $ "The 10000 record limit for visualizations has been exceeded - the current dataset contains "
    ⊕ show numRecords
    ⊕ " records. "
    ⊕ "Please consider using a 'limit' or 'group by' clause in the query to reduce the result size."

  records ←
    CET.liftQ $ QQ.all resource

  pure $ Port.ChartInstructions (buildPie conf records) Pie


buildPie ∷ PieR → JArray → DSL OptionI
buildPie r records = do
  pure unit
