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

import Data.Variant (on)

import SlamData.Workspace.Card.Ace.Component (aceComponent)
import SlamData.Workspace.Card.Cache.Component (cacheCardComponent)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component (CardComponent)
import SlamData.Workspace.Card.Download.Component (downloadComponent)
import SlamData.Workspace.Card.DownloadOptions.Component as DOpts
import SlamData.Workspace.Card.Draftboard.Component (draftboardComponent)
import SlamData.Workspace.Card.Markdown.Component (markdownComponent)
import SlamData.Workspace.Card.Open.Component (openComponent)
import SlamData.Workspace.Card.Search.Component (searchComponent)
import SlamData.Workspace.Card.StructureEditor.Component as StructureEditor
import SlamData.Workspace.Card.Table.Component (tableComponent)
import SlamData.Workspace.Card.Tabs.Component (tabsComponent)
import SlamData.Workspace.Card.Troubleshoot.Component (troubleshootComponent)
import SlamData.Workspace.Card.Variables.Component (variablesComponent)
import SlamData.Workspace.Card.Viz.Component as Viz

import Unsafe.Coerce (unsafeCoerce)

hole ∷ ∀ a. a
hole = unsafeCoerce 1

cardComponent ∷ CT.CardType → CardOptions → CardComponent
cardComponent = case_
  # on CT._aceSql (const $ aceComponent CT.aceSql)
  # on CT._aceMarkdown (const $ aceComponent CT.aceMarkdown)
  # on CT._search (const searchComponent)
  # on CT._markdown (const markdownComponent)
  # on CT._table (const tableComponent)
  # on CT._download (const downloadComponent)
  # on CT._variables (const variablesComponent)
  # on CT._troubleshoot (const troubleshootComponent)
  # on CT._cache (const cacheCardComponent)
  # on CT._open (const openComponent)
  # on CT._downloadOptions (const DOpts.component)
  # on CT._draftboard (const draftboardComponent)
  # on CT._tabs (const tabsComponent)
  # on CT._structureEditor (const StructureEditor.component)
  # on CT._viz  (const Viz.component)
  # on CT._setupViz (const hole)
