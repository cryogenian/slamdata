{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Card.Eval
  ( runCard
  , module SlamData.Workspace.Card.Eval.Transition
  ) where

import SlamData.Prelude

import Control.Monad.Eff as Eff
import Control.Monad.Aff.Free (class Affable, fromEff)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Throw (class MonadThrow, throw)
import Control.Monad.Writer.Class (class MonadWriter)

import Data.Lens ((^?))
import Data.Path.Pathy as Path
import Data.StrMap as SM

import Quasar.Types (SQL, FilePath)

import SlamData.Effects (SlamDataEffects)
import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.Quasar.Error as QE
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Cache.Eval as Cache
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.Transition (Eval(..), tagEval)
import SlamData.Workspace.Card.Markdown.Component.State.Core as MDS
import SlamData.Workspace.Card.Markdown.Eval as MDE
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Search.Interpret as Search
import SlamData.Workspace.Card.Variables.Eval as VariablesE
import SlamData.Workspace.Card.BuildChart.Metric.Eval as BuildMetric
import SlamData.Workspace.Card.BuildChart.Sankey.Eval as BuildSankey
import SlamData.Workspace.Card.BuildChart.Gauge.Eval as BuildGauge
import SlamData.Workspace.Card.BuildChart.Graph.Eval as BuildGraph
import SlamData.Workspace.Card.BuildChart.Pie.Eval as BuildPie
import SlamData.Workspace.Card.BuildChart.Radar.Eval as BuildRadar
import SlamData.Workspace.Card.BuildChart.Area.Eval as BuildArea
import SlamData.Workspace.Card.BuildChart.Line.Eval as BuildLine
import SlamData.Workspace.Card.BuildChart.Bar.Eval as BuildBar
import SlamData.Workspace.Card.BuildChart.Scatter.Eval as BuildScatter
import SlamData.Workspace.Card.BuildChart.Funnel.Eval as BuildFunnel
import SlamData.Workspace.Card.BuildChart.Heatmap.Eval as BuildHeatmap
import SlamData.Workspace.Card.BuildChart.Boxplot.Eval as BuildBoxplot
import SlamData.Workspace.Card.BuildChart.PivotTable.Eval as BuildPivotTable
import SlamData.Workspace.Card.BuildChart.PunchCard.Eval as BuildPunchCard
import SlamData.Workspace.Card.BuildChart.Candlestick.Eval as BuildCandlestick
import SlamData.Workspace.Card.BuildChart.Parallel.Eval as BuildParallel

import Text.SlamSearch as SS
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.State as SDH

runCard
  ∷ ∀ f m
  . ( Applicative m
    , Affable SlamDataEffects m
    , Parallel f m
    , QuasarDSL m
    )
  ⇒ CEM.CardEnv
  → CEM.CardState
  → Port.Port
  → Eval
  → m (CEM.CardResult Port.Port)
runCard env state input trans =
  CEM.runCardEvalM env state (evalCard input trans ∷ CEM.CardEval Port.Port)

evalCard
  ∷ ∀ f m
  . ( Affable SlamDataEffects m
    , Parallel f m
    , MonadReader CEM.CardEnv m
    , MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , MonadWriter CEM.CardLog m
    , QuasarDSL m
    )
  ⇒ Port.Port
  → Eval
  → m Port.Port
evalCard = flip case _, _ of
  Error msg, _ →
    pure $ Port.CardError msg
  Pass, Port.Initial →
    CEM.throw "Cannot pass initial port"
  Pass, Port.Terminal →
    CEM.throw "Cannot pass terminal port"
  Pass, port →
    pure port
  Draftboard, _ →
    pure Port.Draftboard
  Query sql, Port.VarMap varMap →
    Port.TaggedResource <$> evalQuery sql varMap
  Query sql, _ →
    Port.TaggedResource <$> evalQuery sql Port.emptyVarMap
  Markdown txt, port →
    MDE.markdownEval port txt
  MarkdownForm model, Port.SlamDown doc →
    Port.VarMap <$> evalMarkdownForm doc model
  Search query, Port.TaggedResource { resource } →
    Port.TaggedResource <$> evalSearch query resource
  Cache pathString, port@Port.TaggedResource { resource, varMap } →
    Port.TaggedResource <$> Cache.eval port pathString resource varMap
  Open res, _ →
    Port.TaggedResource <$> evalOpen res
  Variables model, _ → do
    Port.VarMap <$> VariablesE.eval model
  DownloadOptions { compress, options }, Port.TaggedResource { resource } →
    pure $ Port.DownloadOptions { resource, compress, options }
  BuildMetric model, Port.TaggedResource tr →
    BuildMetric.eval tr model
  BuildSankey model, Port.TaggedResource tr →
    BuildSankey.eval tr model
  BuildGauge model, Port.TaggedResource tr →
    BuildGauge.eval tr model
  BuildGraph model, Port.TaggedResource tr →
    BuildGraph.eval tr model
  BuildPie model, Port.TaggedResource tr →
    BuildPie.eval tr model
  BuildRadar model, Port.TaggedResource tr →
    BuildRadar.eval tr model
  BuildArea model, Port.TaggedResource tr →
    BuildArea.eval tr model
  BuildLine model, Port.TaggedResource tr →
    BuildLine.eval tr model
  BuildBar model, Port.TaggedResource tr →
    BuildBar.eval tr model
  BuildScatter model, Port.TaggedResource tr →
    BuildScatter.eval tr model
  BuildFunnel model, Port.TaggedResource tr →
    BuildFunnel.eval tr model
  BuildHeatmap model, Port.TaggedResource tr →
    BuildHeatmap.eval tr model
  BuildBoxplot model, Port.TaggedResource tr →
    BuildBoxplot.eval tr model
  BuildPivotTable model, Port.TaggedResource tr →
    BuildPivotTable.eval tr model
  BuildPunchCard model, Port.TaggedResource tr →
    BuildPunchCard.eval tr model
  BuildCandlestick model, Port.TaggedResource tr →
    BuildCandlestick.eval tr model
  BuildParallel model, Port.TaggedResource tr →
    BuildParallel.eval tr model
  e, i →
    CEM.throw $ "Card received unexpected input type; " <> tagEval e <> " | " <> Port.tagPort i

evalMarkdownForm
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , Monad m
    )
  ⇒ Port.VarMap × SD.SlamDownP Port.VarMapValue
  → MD.Model
  → m Port.VarMap
evalMarkdownForm (vm × doc) model = do
  let inputState = SDH.formStateFromDocument doc
  -- TODO: find a way to smash these annotations if possible -js
  thisVarMap ←
    fromEff (MDS.formStateToVarMap inputState model.state ∷ Eff.Eff SlamDataEffects Port.VarMap)
  pure $ thisVarMap `SM.union` vm

evalOpen
  ∷ ∀ m
  . ( MonadThrow CEM.CardError m
    , MonadWriter CEM.CardLog m
    , QuasarDSL m
    )
  ⇒ R.Resource
  → m Port.TaggedResourcePort
evalOpen res = do
  filePath ←
    maybe (CEM.throw "No resource is selected") pure
      $ res ^? R._filePath
  msg ←
    CEM.liftQ $ QFS.messageIfFileNotFound filePath $
      "File " ⊕ Path.printPath filePath ⊕ " doesn't exist"
  case msg of
    Nothing → do
      CEM.addSource filePath
      pure { resource: filePath, tag: Nothing, varMap: Nothing }
    Just err →
      CEM.throw err

evalQuery
  ∷ ∀ f m
  . ( Affable SlamDataEffects m
    , Parallel f m
    , MonadReader CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadWriter CEM.CardLog m
    , QuasarDSL m
    )
  ⇒ SQL
  → Port.VarMap
  → m Port.TaggedResourcePort
evalQuery sql varMap = do
  urlVarMap ← CEM.localUrlVarMap
  resource ← CEM.temporaryOutputResource
  let
    varMap' =
      SM.union urlVarMap (Port.renderVarMapValue <$> varMap)
    backendPath =
      Left $ fromMaybe Path.rootDir (Path.parentDir resource)
  { inputs } ←
    CEM.liftQ $ lmap (QE.prefixMessage "Error compiling query") <$>
      QQ.compile backendPath sql varMap'
  validateResources inputs
  CEM.addSources inputs
  pure { resource, tag: pure sql, varMap: Just varMap }

evalSearch
  ∷ ∀ f m
  . ( Affable SlamDataEffects m
    , Parallel f m
    , MonadReader CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadWriter CEM.CardLog m
    , QuasarDSL m
    )
  ⇒ String
  → FilePath
  → m Port.TaggedResourcePort
evalSearch queryText resource = do
  query ← case SS.mkQuery queryText of
    Left _ → CEM.throw "Incorrect query string"
    Right q → pure q

  fields ← CEM.liftQ do
    QFS.messageIfFileNotFound
      resource
      ("Input resource " ⊕ Path.printPath resource ⊕ " doesn't exist")
    QQ.fields resource

  outputResource ← CEM.temporaryOutputResource

  let
    template = Search.queryToSQL fields query
    sql = QQ.templated resource template

  compileResult ← QQ.compile (Right resource) sql SM.empty
  case compileResult of
    Left err →
      case GE.fromQError err of
        Left msg → CEM.throw $ "Error compiling query: " ⊕ msg
        Right _ → CEM.throw $ "Error compiling query: " ⊕ QE.printQError err
    Right { inputs } → do
      validateResources inputs
      CEM.addSources inputs

  pure { resource: outputResource, tag: pure sql, varMap: Nothing }

validateResources
  ∷ ∀ f m t
  . ( Affable SlamDataEffects m
    , Parallel f m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    , Foldable t
    )
  ⇒ t FilePath
  → m Unit
validateResources =
  parTraverse_ \path → do
    noAccess ← QFS.fileNotAccessible path
    for_ noAccess \reason →
      throw $ QE.prefixMessage ("Resource `" ⊕ Path.printPath path ⊕ "` is unavailable") reason
