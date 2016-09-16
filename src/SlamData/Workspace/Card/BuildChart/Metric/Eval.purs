module SlamData.Workspace.Card.BuildChart.Metric.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Metric.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber)
import Data.Array as A
import Data.Formatter.Number as FN
import Data.String as Str
import Data.String.Regex as Rgx
import Data.Lens ((^?))
import Data.Lens as Lens

import Quasar.Types (FilePath)

import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.BuildChart.Metric.Model (Model, MetricR)
import SlamData.Workspace.Card.Chart.Aggregation as Ag


eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _  =
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

  pure $ Port.Metric $ buildMetric conf records


buildMetric ∷ MetricR → JArray → Port.MetricPort
buildMetric r records =
  { value, label: r.label }
  where
  formatterRegex ∷ Rgx.Regex
  formatterRegex =
    unsafePartial fromRight $ Rgx.regex "{{[^}]+}}" Rgx.noFlags

  value ∷ String
  value = fromMaybe (show metricValue) do
    input ← r.formatter
    matches ← Rgx.match formatterRegex input
    firstMatch ← join $ A.head matches
    woPrefix ← Str.stripPrefix "{{" firstMatch
    formatString ← Str.stripSuffix "}}" woPrefix
    formatter ← either (const Nothing) Just $ FN.parseFormatString formatString
    let
      formattedNumber = FN.format formatter metricValue

    pure $ Rgx.replace formatterRegex formattedNumber input

  metricValue ∷ Number
  metricValue =
    Ag.runAggregation r.valueAggregation $ foldMap foldFn records

  foldFn ∷ Json → Array Number
  foldFn js =
    foldMap A.singleton $ toNumber =<< cursorGet r.value js
