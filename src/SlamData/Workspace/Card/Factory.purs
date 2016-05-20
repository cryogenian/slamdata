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

-- haha only serious
module SlamData.Workspace.Card.Factory where

import SlamData.Prelude

import Data.BrowserFeatures (BrowserFeatures)

import SlamData.Workspace.Card.Ace.Component (AceEvaluator, AceSetup, aceComponent)
import SlamData.Workspace.Card.API.Component (apiComponent)
import SlamData.Workspace.Card.APIResults.Component (apiResultsComponent)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType (CardType(..), AceMode(..))
import SlamData.Workspace.Card.Chart.Component (chartComponent)
import SlamData.Workspace.Card.Component (CardComponent)
import SlamData.Workspace.Card.Download.Component (downloadComponent)
import SlamData.Workspace.Card.DownloadOptions.Component as DOpts
import SlamData.Workspace.Card.Error.Component as Error
import SlamData.Workspace.Card.JTable.Component (jtableComponent)
import SlamData.Workspace.Card.Markdown.Component (markdownComponent)
import SlamData.Workspace.Card.Markdown.Eval (markdownEval, markdownSetup)
import SlamData.Workspace.Card.Next.Component (nextCardComponent)
import SlamData.Workspace.Card.OpenResource.Component (openResourceComponent)
import SlamData.Workspace.Card.Query.Eval (queryEval, querySetup)
import SlamData.Workspace.Card.Save.Component (saveCardComponent)
import SlamData.Workspace.Card.Search.Component (searchComponent)
import SlamData.Workspace.Card.Viz.Component (vizComponent)

cardTypeComponent ∷ CardType → CardId → BrowserFeatures → CardComponent
cardTypeComponent (Ace mode) _ _ = aceComponent { mode, evaluator, setup }
  where
  evaluator = aceEvalMode mode
  setup = aceSetupMode mode
cardTypeComponent Search _ _ = searchComponent
cardTypeComponent Viz _ _ = vizComponent
cardTypeComponent Chart _ _ = chartComponent
cardTypeComponent Markdown cardId bf = markdownComponent cardId bf
cardTypeComponent JTable _ _ = jtableComponent
cardTypeComponent Download _ _ = downloadComponent
cardTypeComponent API _ _ = apiComponent
cardTypeComponent APIResults _ _ = apiResultsComponent
cardTypeComponent NextAction _ _ = nextCardComponent
cardTypeComponent Save _ _ = saveCardComponent
cardTypeComponent OpenResource _ _ = openResourceComponent
cardTypeComponent DownloadOptions _ _ = DOpts.comp
cardTypeComponent ErrorCard _ _ = Error.comp

aceEvalMode ∷ AceMode → AceEvaluator
aceEvalMode MarkdownMode = markdownEval
aceEvalMode SQLMode = queryEval

aceSetupMode ∷ AceMode → AceSetup
aceSetupMode MarkdownMode = markdownSetup
aceSetupMode SQLMode = querySetup
