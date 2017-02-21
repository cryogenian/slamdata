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

module SlamData.Workspace.MillerColumns.Component
  ( component
  , module SlamData.Workspace.MillerColumns.Component.Query
  , module SlamData.Workspace.MillerColumns.Component.State
  , module Exports
  ) where

import SlamData.Prelude

import Control.Monad.Eff (Eff)

import Data.List ((:))
import Data.List as L

import DOM (DOM)
import DOM.Classy.HTMLElement as DOM
import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Column.Component as Column
import SlamData.Workspace.MillerColumns.Component.Query (Query(..), Message(..), Message')
import SlamData.Workspace.MillerColumns.Component.State (State, columnPaths)

import SlamData.Workspace.MillerColumns.Column.Component (ColumnOptions) as Exports

type HTML a i o = H.ParentHTML (Query a i o) (Column.Query a i o) (L.List i) Slam
type DSL a i o = H.ParentDSL (State i) (Query a i o) (Column.Query a i o) (L.List i) (Message' a i o) Slam

type RenderRec a i o = { path ∷ L.List i, html ∷ Array (HTML a i o) }

component
  ∷ ∀ a i f o
  . Ord i
  ⇒ Column.ColumnOptions a i f o
  → H.Component HH.HTML (Query a i o) (L.List i) (Message' a i o) Slam
component colSpec =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: HE.input ChangeRoot
    }
  where

  render ∷ State i → HTML a i o
  render state =
    HH.div
      [ HP.class_ (HH.ClassName "sd-miller-columns")
      , HP.ref containerRef
      ]
      $ _.html
      $ foldr goColumn { path: L.Nil, html: [] } (columnPaths colSpec state)

  goColumn ∷ L.List i → RenderRec a i o → RenderRec a i o
  goColumn path acc = { path, html: acc.html <> [renderColumn path] }

  renderColumn ∷ L.List i → HTML a i o
  renderColumn colPath =
    HH.div
      [ HP.class_ (HH.ClassName "sd-miller-column")
      , ARIA.label "Column"
      ]
      [ HH.slot colPath (Column.component colSpec colPath) unit (HE.input (HandleMessage colPath)) ]

  eval ∷ Query a i o ~> DSL a i o
  eval = case _ of
    Populate path next → do
      H.put path
      pure next
    ChangeRoot root next → do
      currentPath ← H.get
      let prefix = L.take (L.length root) root
      when (prefix /= root) $ H.put root
      pure next
    HandleMessage colPath msg next → do
      case msg of
        Left Column.Initialized →
          traverse_ (H.liftEff ∘ scrollToRight) =<< H.getHTMLElementRef containerRef
        Left Column.Deselected → do
          H.put colPath
          void $ H.query colPath $ H.action $ Column.SetSelection Nothing
          let prevCol = L.drop 1 colPath
          selection ← join <$> H.query prevCol (H.request Column.GetSelection)
          let selPath = maybe prevCol (\s → colSpec.id s : prevCol) selection
          H.raise $ Left (SelectionChanged selPath selection)
        Left (Column.Selected itemPath selection) → do
          H.put itemPath
          void $ H.query colPath $ H.action $ Column.SetSelection (Just selection)
          H.raise $ Left (SelectionChanged itemPath (Just selection))
        Right o →
          H.raise (Right o)
      pure next

scrollToRight ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Unit
scrollToRight el = do
  maxScroll ← (-) <$> DOM.scrollWidth el <*> DOM.offsetWidth el
  DOM.setScrollLeft maxScroll el

containerRef ∷ H.RefLabel
containerRef = H.RefLabel "container"
