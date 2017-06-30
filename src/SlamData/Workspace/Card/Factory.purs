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

import SlamData.Workspace.Card.Ace.Component (aceComponent)
import SlamData.Workspace.Card.Cache.Component (cacheCardComponent)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as ChT
import SlamData.Workspace.Card.CardType.FormInputType as FiT
import SlamData.Workspace.Card.CardType.GeoChartType as GcT
import SlamData.Workspace.Card.Chart.Component (chartComponent)
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component (CardComponent)
import SlamData.Workspace.Card.Download.Component (downloadComponent)
import SlamData.Workspace.Card.DownloadOptions.Component as DOpts
import SlamData.Workspace.Card.Draftboard.Component (draftboardComponent)
import SlamData.Workspace.Card.FormInput.Component (formInputComponent)
import SlamData.Workspace.Card.Markdown.Component (markdownComponent)
import SlamData.Workspace.Card.Open.Component (openComponent)
import SlamData.Workspace.Card.Search.Component (searchComponent)
import SlamData.Workspace.Card.Geo.Component as Geo
import SlamData.Workspace.Card.StructureEditor.Component as StructureEditor
import SlamData.Workspace.Card.Table.Component (tableComponent)
import SlamData.Workspace.Card.Tabs.Component (tabsComponent)
import SlamData.Workspace.Card.Troubleshoot.Component (troubleshootComponent)
import SlamData.Workspace.Card.Variables.Component (variablesComponent)
import SlamData.Workspace.Card.Setups.Viz.Component as SetupViz
import SlamData.Workspace.Card.Viz.Component as Viz


cardComponent ∷ CT.CardType → CardOptions → CardComponent
cardComponent = case _ of
  CT.Ace mode → aceComponent mode
  CT.Search → searchComponent
  CT.Chart → chartComponent
  CT.Markdown → markdownComponent
  CT.Table → tableComponent
  CT.Download → downloadComponent
  CT.Variables → variablesComponent
  CT.Troubleshoot → troubleshootComponent
  CT.Cache → cacheCardComponent
  CT.Open → openComponent
  CT.DownloadOptions → DOpts.component
  CT.Draftboard → draftboardComponent
  CT.FormInput → formInputComponent
  CT.Tabs → tabsComponent
  CT.StructureEditor → StructureEditor.component
  CT.GeoChart → Geo.component
  CT.SetupViz → SetupViz.component
  CT.Viz → Viz.component
