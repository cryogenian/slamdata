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
  , modelToEval
  , module SlamData.Workspace.Card.Eval.Transition
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer.Class (class MonadTell)
import Data.StrMap as SM
import Data.List ((:))
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Workspace.Card.Cache.Eval as Cache
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Eval as PivotTable
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Model as PTM
import SlamData.Workspace.Card.DownloadOptions.Eval as DOptions
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as Common
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.Transition (Eval(..), tagEval)
import SlamData.Workspace.Card.Markdown.Eval as MDE
import SlamData.Workspace.Card.Model as Model
import SlamData.Workspace.Card.Open.Eval as Open
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Query.Eval as Query
import SlamData.Workspace.Card.Search.Eval as Search
import SlamData.Workspace.Card.Setups.Viz.Eval as SetupViz
import SlamData.Workspace.Card.Geo.Eval as Geo
import SlamData.Workspace.Card.Table.Eval as Table
import SlamData.Workspace.Card.Variables.Eval as VariablesE

runCard
  ∷ ∀ f m
  . MonadAff SlamDataEffects m
  ⇒ QuasarDSL m
  ⇒ Parallel f m
  ⇒ Monad m
  ⇒ CEM.CardEnv
  → CEM.CardState
  → Eval
  → Port.Port
  → Port.DataMap
  → m (CEM.CardResult CE.CardError Port.Out)
runCard env state trans input varMap =
  CEM.runCardEvalM env state (evalCard trans input varMap ∷ CEM.CardEval CE.CardError Port.Out)


evalCard
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ Eval
  → Port.Port
  → Port.DataMap
  → m Port.Out
evalCard trans port varMap = map (_ `SM.union` varMap) <$> case trans, port of
  Error msg, _ → CE.throw msg
  _, Port.CardError err → throwError err
  Pass, _ → pure (port × varMap)
  Table m, _ → Table.eval m port varMap
  PivotTable m, Port.PivotTable p → PivotTable.eval m p varMap
  Chart, Port.PivotTable p → PivotTable.eval PTM.initialModel p varMap
  Chart, _ → pure (Port.ResourceKey Port.defaultResourceVar × varMap)
  GeoChart, Port.GeoChart m → tapResource (Geo.eval m) varMap
  Composite, _ → Port.varMapOut <$> Common.evalComposite
  Terminal, _ → pure Port.terminalOut
  Query sql, _ → Query.evalQuery sql varMap
  Markdown txt, _ → MDE.evalMarkdown txt varMap
  MarkdownForm model, Port.SlamDown doc → MDE.evalMarkdownForm model doc varMap
  Search query, _ → Search.evalSearch query =<< extractResource varMap
  Cache path, _ → Cache.eval path =<< extractResource varMap
  Open res, _ → Open.evalOpen res varMap
  Variables model, _ → VariablesE.eval model
  SetupViz model, _ → SetupViz.eval model =<< extractResource varMap
  FormInput _, _ → pure (Port.ResourceKey Port.defaultResourceVar × varMap)
  DownloadOptions model, _ → tapResource (DOptions.eval model) varMap

  e, i → CE.throw $ "Card received unexpected input type; " <> tagEval e <> " | " <> Port.tagPort i

modelToEval ∷ Model.AnyCardModel → Eval
modelToEval = case _ of
  Model.Ace CT.SQLMode model → Query model.text
  Model.Ace CT.MarkdownMode model → Markdown model.text
  Model.Markdown model → MarkdownForm model
  Model.Search txt → Search txt
  Model.Cache fp → Cache fp
  Model.Open res → Open res
  Model.Variables model → Variables model
  Model.DownloadOptions model → DownloadOptions model
  Model.Draftboard _ → Composite
  Model.Chart Nothing → Chart
  Model.FormInput model → FormInput model
  Model.Tabs _ → Terminal
  Model.Table model → Table model
  Model.Geo model → GeoChart
  Model.SetupViz model → SetupViz model
  _ → Pass

-- TODO(Christoph): Get rid of this monstrosity of an error message
extractResourceVar ∷ ∀ m. MonadThrow CE.CardError m ⇒ Port.DataMap → m (String × Port.Resource)
extractResourceVar dm = case SM.toUnfoldable (Port.filterResources dm) of
  _ : _ : _ → CE.throw "Multiple resources selected"
  r : _ → pure r
  _ → CE.throw "No resource selected"

extractResource ∷ ∀ m. MonadThrow CE.CardError m ⇒ Port.DataMap → m (Port.Resource)
extractResource = map snd ∘ extractResourceVar

tapResource
  ∷ ∀ m
  . MonadThrow CE.CardError m
  ⇒ (Port.Resource → m Port.Port)
  → Port.DataMap
  → m Port.Out
tapResource f dm =
  map (_ × dm) (f =<< extractResource dm)
