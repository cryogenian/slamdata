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
import DOM.HTML.HTMLElement (offsetWidth) as DOM
import DOM.HTML.Types (HTMLElement, htmlElementToElement)
import DOM.Node.Element (scrollWidth, setScrollLeft) as DOM

import Halogen as H
import Halogen.Component.Utils (raise')
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Column.Component as Column
import SlamData.Workspace.MillerColumns.Component.Query (Query(..), Query')
import SlamData.Workspace.MillerColumns.Component.State (State, State', columnPaths, initialState)

import SlamData.Workspace.MillerColumns.Column.Component (ColumnOptions, InitialItemState(..), ItemQuery(..), ItemQuery', ItemHTML) as Exports

type HTML a i s f = H.ParentHTML (Column.State' a i s f) (Query a i) (Column.Query' a i f) Slam (L.List i)
type DSL a i s f = H.ParentDSL (State a i) (Column.State' a i s f) (Query a i) (Column.Query' a i f) Slam (L.List i)

type RenderRec a i s f = { path ∷ L.List i, html ∷ Array (HTML a i s f) }

component
  ∷ ∀ a i s f
  . Ord i
  ⇒ Column.ColumnOptions a i s f
  → H.Component (State' a i s f) (Query' a i f) Slam
component colSpec = H.parentComponent { render, eval, peek: Just peek }
  where

  render ∷ State a i → HTML a i s f
  render state =
    HH.div
      [ HP.class_ (HH.className "sd-miller-columns")
      , HP.ref (H.action ∘ Ref)
      ]
      $ _.html
      $ foldr goColumn { path: L.Nil, html: [] } (columnPaths colSpec state)

  goColumn ∷ L.List i → RenderRec a i s f → RenderRec a i s f
  goColumn path acc = { path, html: acc.html <> [renderColumn path] }

  renderColumn ∷ L.List i → HTML a i s f
  renderColumn colPath =
    HH.div
      [ HP.class_ (HH.className "sd-miller-column")
      , ARIA.label "Column"
      , HP.ref (\_ → H.action Extended)
      ]
      [ HH.slot colPath \_ →
          { component: Column.component colSpec colPath
          , initialState: H.parentState Column.initialState
          }
      ]

  eval ∷ Query a i ~> DSL a i s f
  eval = case _ of
    Ref mel next → do
      H.modify (_ { element = mel })
      pure next
    Extended next → do
      traverse_ (H.fromEff ∘ scrollToRight) =<< H.gets _.element
      pure next
    Populate path next → do
      H.modify (_ { path = path })
      pure next
    RaiseSelected _ _ next → do
      pure next

  peek ∷ ∀ x. H.ChildF (L.List i) (Column.Query' a i f) x → DSL a i s f Unit
  peek (H.ChildF colPath q) =
    coproduct (peekColumn colPath) (peekColumnItem colPath) q

  peekColumn
    ∷ ∀ x
    . L.List i
    → Column.Query a x
    → DSL a i s f Unit
  peekColumn colPath = case _ of
    Column.Deselect _ → do
      raise' $ H.action $ Populate colPath
      void $ H.query colPath $ left $ H.action $ Column.SetSelection Nothing
      let prevCol = L.drop 1 colPath
      selection ← join <$> H.query prevCol (left (H.request Column.GetSelection))
      let selPath = maybe prevCol (\s → colSpec.id s : prevCol) selection
      raise' $ H.action $ RaiseSelected selPath selection
    _ → pure unit

  peekColumnItem
    ∷ ∀ x
    . L.List i
    → H.ChildF i (Column.ItemQuery' a f) x
    → DSL a i s f Unit
  peekColumnItem colPath (H.ChildF itemId q) =
    coproduct (peekColumnItemCommon itemId colPath) (const (pure unit)) q

  peekColumnItemCommon
    ∷ ∀ x
    . i
    → L.List i
    → Column.ItemQuery a x
    → DSL a i s f Unit
  peekColumnItemCommon itemId colPath = case _ of
    Column.RaisePopulate selection _ → do
      let itemPath = itemId : colPath
      raise' $ H.action $ Populate itemPath
      raise' $ H.action $ RaiseSelected itemPath (Just selection)
      void $ H.query colPath $ left $ H.action $ Column.SetSelection (Just selection)
    _ → pure unit

scrollToRight ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Unit
scrollToRight hel = do
  let el = htmlElementToElement hel
  maxScroll ← (-) <$> DOM.scrollWidth el <*> DOM.offsetWidth hel
  DOM.setScrollLeft maxScroll el
