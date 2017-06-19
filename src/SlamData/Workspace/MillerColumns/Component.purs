{-
Copyright 2017 SlamData, Inc.

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
  ( MillerColumnsComponent
  , component
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
import DOM.Classy.Event (preventDefault) as DOM
import DOM.Classy.HTMLElement (offsetHeight, scrollHeight, setScrollTop) as DOM
import DOM.HTML.Types (HTMLElement)
import Halogen as H
import Halogen.Component.Proxy (queryQ)
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Column.Component (ColumnOptions(..)) as Exports
import SlamData.Workspace.MillerColumns.Column.Component as Column
import SlamData.Workspace.MillerColumns.Column.Component.Request (LoadRequest, LoadResponse) as Exports
import SlamData.Workspace.MillerColumns.Component.Query (Query(..), Message(..), Message')
import SlamData.Workspace.MillerColumns.Component.State as S

type MillerColumnsComponent a i o = H.Component HH.HTML (Query a i o) (S.ColumnsData a i) (Message' a i o) Slam

type HTML a i o = H.ParentHTML (Query a i o) (Column.Query' a i o) (Int × i) Slam
type DSL a i o = H.ParentDSL (S.State a i) (Query a i o) (Column.Query' a i o) (Int × i) (Message' a i o) Slam

component
  ∷ ∀ a i o
  . Ord i
  ⇒ Column.ColumnOptions a i o
  → MillerColumnsComponent a i o
component opts@(Column.ColumnOptions colSpec) =
  H.parentComponent
    { initialState: { cycle: 0, widths: L.Nil, columns: _ }
    , render
    , eval
    , receiver: HE.input ChangeRoot
    }
  where

  render ∷ S.State a i → HTML a i o
  render state =
    HH.div
      [ HP.class_ (HH.ClassName "sd-miller-columns")
      , HP.ref containerRef
      ]
      $ join
      $ A.fromFoldable
      $ map (renderColumn state) (S.columnPaths opts state.columns)

  renderColumn ∷ S.State a i → Int × Maybe a × i → Array (HTML a i o)
  renderColumn st@{ cycle } (ix × sel × colPath) =
    [ HH.div
        [ HP.classes $ join
            [ pure $ HH.ClassName "sd-miller-column"
            , guard (ix == L.length (snd st.columns) - 1) $> HH.ClassName "sd-miller-column-last"
            ]
        , ARIA.label "Column"
        ]
        [ HH.slot
            (cycle × colPath)
            (colSpec.renderColumn opts colPath)
            (S.columnWidth st ix × sel)
            (HE.input (HandleMessage ix colPath))
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "sd-miller-column-resizer")
        , HE.onMouseDown (HE.input (DragStart ix))
        ]
        []
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
        Left Column.Initialized → do
          traverse_ (H.liftEff ∘ scrollToRight) =<< H.getHTMLElementRef containerRef
        Left Column.Deselected → do
          H.modify $ S.modifyColumns $ second \sels →
            L.drop (L.length sels - colIndex) sels
          state ← H.gets _.columns
          root × sel ← pure (second L.head state)
          H.raise $ Left (SelectionChanged state (maybe root colSpec.id sel) sel)
        Left (Column.Selected itemPath item) → do
          H.modify $ S.modifyColumns $ second \sels →
            item : L.drop (L.length sels - colIndex) sels
          state ← H.gets _.columns
          H.raise $ Left (SelectionChanged state itemPath (Just item))
        Left (Column.LoadRequest req) → do
          H.raise $ Left (LoadRequest (colPath × req))
        Right o →
          H.raise (Right o)
      pure next
    Reload next → do
      H.modify \st → st { cycle = st.cycle + 1 }
      pure next
    FulfilLoadRequest (colPath × response) next → do
      cycle ← H.gets _.cycle
      _ ← H.query (cycle × colPath) $ queryQ $ H.action $ Column.FulfilLoadRequest response
      pure next
    DragStart ix ev next → do
      H.liftEff $ DOM.preventDefault ev
      H.modify (S.fixColumnWidths opts)
      colWidth ← H.gets (flip S.columnWidth ix)
      H.subscribe $ Drag.dragEventSource ev \drag →
        Just (DragUpdate ix colWidth drag H.Listening)
      pure next
    DragUpdate ix (Column.ColumnWidth origWidth) de next → do
      case de of
        Drag.Move _ { offsetX } → do
          let newWidth = max Column.minColumnWidth $ Column.ColumnWidth (origWidth + offsetX)
          H.modify (\st → st { widths = fromMaybe st.widths $ L.modifyAt ix (const newWidth) st.widths })
        _ → pure unit
      pure next

scrollToRight ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Unit
scrollToRight el = do
  maxScroll ← (-) <$> DOM.scrollHeight el <*> DOM.offsetHeight el
  DOM.setScrollTop maxScroll el

containerRef ∷ H.RefLabel
containerRef = H.RefLabel "container"
