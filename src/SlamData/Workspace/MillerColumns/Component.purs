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
  , module Exports
  ) where

import SlamData.Prelude

import Control.Monad.Eff (Eff)

import Data.Array as A
import Data.List ((:))
import Data.List as L
import Data.Profunctor.Strong (second)

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
import SlamData.Workspace.MillerColumns.Component.State (State, ColumnsData, modifyColumns, columnPaths)

import SlamData.Workspace.MillerColumns.Column.Component (ColumnOptions(..)) as Exports

type HTML a i o = H.ParentHTML (Query a i o) (Column.Query' a i o) (Int × i) Slam
type DSL a i o = H.ParentDSL (State a i) (Query a i o) (Column.Query' a i o) (Int × i) (Message' a i o) Slam

component
  ∷ ∀ a i o
  . Ord i
  ⇒ Column.ColumnOptions a i o
  → H.Component HH.HTML (Query a i o) (ColumnsData a i) (Message' a i o) Slam
component opts@(Column.ColumnOptions colSpec) =
  H.parentComponent
    { initialState: { cycle: 0, columns: _ }
    , render
    , eval
    , receiver: HE.input ChangeRoot
    }
  where

  render ∷ State a i → HTML a i o
  render state =
    HH.div
      [ HP.class_ (HH.ClassName "sd-miller-columns")
      , HP.ref containerRef
      ]
      $ A.fromFoldable
      $ map (renderColumn state.cycle) (columnPaths opts state.columns)

  renderColumn ∷ Int → Int × Maybe a × i → HTML a i o
  renderColumn cycle (i × sel × colPath) =
    HH.div
      [ HP.class_ (HH.ClassName "sd-miller-column")
      , ARIA.label "Column"
      ]
      [ HH.slot
          (cycle × colPath)
          (colSpec.renderColumn opts colPath)
          sel
          (HE.input (HandleMessage i colPath))
      ]

  eval ∷ Query a i o ~> DSL a i o
  eval = case _ of
    Populate cols next → do
      H.modify (_ { columns = cols })
      pure next
    ChangeRoot cols@(newRoot × newSelections) next → do
      (root × selections) ← H.gets _.columns
      let
        prefix = L.drop (L.length selections - L.length newSelections) selections
        oldIds = colSpec.id <$> prefix
        newIds = colSpec.id <$> newSelections
      when (root /= newRoot || oldIds /= newIds) $ H.modify (_ { columns = cols })
      pure next
    HandleMessage colIndex colPath msg next → do
      case msg of
        Left Column.Initialized →
          traverse_ (H.liftEff ∘ scrollToRight) =<< H.getHTMLElementRef containerRef
        Left Column.Deselected → do
          H.modify $ modifyColumns $ second \sels →
            L.drop (L.length sels - colIndex) sels
          root × sel ← second L.head <$> H.gets _.columns
          H.raise $ Left (SelectionChanged (maybe root colSpec.id sel) sel)
        Left (Column.Selected itemPath item) → do
          H.modify $ modifyColumns $ second \sels →
            item : L.drop (L.length sels - colIndex) sels
          H.raise $ Left (SelectionChanged itemPath (Just item))
        Right o →
          H.raise (Right o)
      pure next
    Reload next → do
      H.modify \st → st { cycle = st.cycle + 1 }
      pure next

scrollToRight ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Unit
scrollToRight el = do
  maxScroll ← (-) <$> DOM.scrollWidth el <*> DOM.offsetWidth el
  DOM.setScrollLeft maxScroll el

containerRef ∷ H.RefLabel
containerRef = H.RefLabel "container"
