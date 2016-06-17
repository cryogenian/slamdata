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
module SlamData.Workspace.Card.Factory
  ( cardComponent
  ) where

import SlamData.Prelude

import Halogen as H

import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Ace.Component (AceEval, aceComponent)
import SlamData.Workspace.Card.API.Component (apiComponent)
import SlamData.Workspace.Card.APIResults.Component (apiResultsComponent)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Chart.Component (chartComponent)
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component (CardComponent)
import SlamData.Workspace.Card.Draftboard.Component (draftboardComponent)
import SlamData.Workspace.Card.Download.Component (downloadComponent)
import SlamData.Workspace.Card.DownloadOptions.Component as DOpts
import SlamData.Workspace.Card.Error.Component as Error
import SlamData.Workspace.Card.Pending.Component as Pending
import SlamData.Workspace.Card.JTable.Component (jtableComponent)
import SlamData.Workspace.Card.Markdown.Component (markdownComponent)
import SlamData.Workspace.Card.Next.Component (nextCardComponent)
import SlamData.Workspace.Card.OpenResource.Component (openResourceComponent)
import SlamData.Workspace.Card.Query.Eval (queryEval)
import SlamData.Workspace.Card.Cache.Component (cacheCardComponent)
import SlamData.Workspace.Card.Search.Component (searchComponent)
import SlamData.Workspace.Card.Viz.Component (vizComponent)

cardComponent ∷ Card.Model → CardOptions → CardComponent
cardComponent card opts =
  case card.model of
    Card.Ace mode _ →
      aceComponent
        { mode
        , eval: aceEval mode
        }
    Card.Search _ → searchComponent
    Card.Viz _ → vizComponent
    Card.Chart → chartComponent
    Card.Markdown _ → markdownComponent card.cardId
    Card.JTable _ → jtableComponent
    Card.Download → downloadComponent
    Card.API _ → apiComponent
    Card.APIResults → apiResultsComponent
    Card.NextAction → nextCardComponent
    Card.Cache _ → cacheCardComponent
    Card.OpenResource mres → openResourceComponent mres
    Card.DownloadOptions _ → DOpts.comp
    Card.ErrorCard → Error.comp
    Card.PendingCard → Pending.comp
    Card.Draftboard _ → draftboardComponent opts

aceEval ∷ CT.AceMode → AceEval
aceEval CT.MarkdownMode = const $ H.modify _{isNew = false}
aceEval CT.SQLMode = queryEval
