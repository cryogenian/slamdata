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

module SlamData.Workspace.MillerColumns.Column.Component
  ( component
  , module SlamData.Workspace.MillerColumns.Column.Options
  , module SlamData.Workspace.MillerColumns.Column.Component.Query
  , module SlamData.Workspace.MillerColumns.Column.Component.State
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.List ((:))
import Data.List as L
import Data.Time.Duration (Milliseconds(..))

import DOM.HTML.Types (htmlElementToElement) as DOM
import DOM.Node.Element (scrollTop, scrollHeight, clientHeight) as DOM

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Render.Common as RC
import SlamData.Workspace.MillerColumns.Column.Options (ColumnOptions, InitialItemState(..), ItemHTML, LoadParams)
import SlamData.Workspace.MillerColumns.Column.Component.Query (ItemQuery(..), ItemQuery', Query(..), Query')
import SlamData.Workspace.MillerColumns.Column.Component.State (ColumnState(..), State, State', initialState)

import Utils.Debounced (debouncedEventSource)

type HTML a i s f = H.ParentHTML s (Query a) (ItemQuery' a f) Slam i
type DSL a i s f = H.ParentDSL (State a i) s (Query a) (ItemQuery' a f) Slam i

component
  ∷ ∀ a i s f
  . Ord i
  ⇒ ColumnOptions a i s f
  → L.List i
  → H.Component (State' a i s f) (Query' a i f) Slam
component ispec colPath =
  H.lifecycleParentComponent
    { render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , peek: Nothing
    }
  where

  render ∷ State a i → HTML a i s f
  render { items, state, selected, filterText } =
    let
      listItems = A.fromFoldable (renderItem selected <$> items)
    in
      HH.div_
        [ HH.div
            [ HP.class_ (HH.className "sd-miller-column-filter") ]
            [ HH.div
                [ HP.class_ (HH.className "sd-action-filter-icon") ]
                [ RC.searchFieldIcon ]
            , HH.input
                [ HP.class_ (HH.className "sd-form-input")
                , HP.value filterText
                , HE.onValueInput (HE.input HandleFilterChange)
                ]
            , HH.button
                [ HP.buttonType HP.ButtonButton
                , HE.onClick (HE.input_ (UpdateFilter ""))
                , HP.enabled (filterText /= "")
                ]
                [ RC.clearFieldIcon "Clear filter" ]
            ]
        , renderSelected selected
        , HH.ul
            [ HE.onScroll (HE.input (HandleScroll <<< _.currentTarget)) ]
            $ listItems
            <> (guard (state == Loading) $> loadIndicator)
        ]

  loadIndicator ∷ HTML a i s f
  loadIndicator =
    HH.li
      [ HP.class_ (HH.className "sd-miller-column-loading") ]
      [ HH.span_ [ HH.text "Loading..." ] ]

  renderSelected ∷ Maybe a → HTML a i s f
  renderSelected = case _ of
    Nothing →
      HH.div
        [ HP.class_ (HH.className "sd-miller-column-selection") ]
        [ HH.span_ [ HH.text "No selection" ] ]
    Just x →
      let
        label = ispec.label x
        deselLabel = "Deselect '" <> label <> "'"
      in
        HH.div
          [ HP.classes
              [ HH.className "sd-miller-column-selection"
              , HH.className "selected"
              ]
          , HP.title deselLabel
          , ARIA.label deselLabel
          , HE.onClick $ HE.input_ Deselect
          ]
          [ HH.span
              [ HP.class_ (HH.className "sd-miller-column-selection-label") ]
              [ HH.text label ]
          , RC.clearFieldIcon deselLabel
          ]

  renderItem ∷ Maybe a → a → HTML a i s f
  renderItem selected item =
    let
      itemId = ispec.id item
      selectedId = ispec.id <$> selected
    in
      HH.slot itemId \_ →
        ispec.render
          (itemId : colPath)
          item
          (if Just itemId == selectedId then Selected else Deselected)

  eval ∷ Query a ~> DSL a i s f
  eval = case _ of
    Init next → do
      trigger ← debouncedEventSource H.subscribe' (Milliseconds 750.0)
      H.modify (_ { filterTrigger = trigger })
      load
      pure next
    SetSelection selected next → do
      H.gets _.selected >>= traverse \s →
        H.query (ispec.id s) $ left $ H.action $ ToggleHighlight false
      H.modify (_ { selected = selected })
      for_ selected \s →
        H.query (ispec.id s) $ left $ H.action $ ToggleHighlight true
      pure next
    GetSelection k →
      k <$> H.gets _.selected
    Deselect next → do
      pure next
    HandleFilterChange text next → do
      trigger ← H.gets _.filterTrigger
      H.modify (\st → st { tick = st.tick + 1, filterText = text })
      H.liftH $ H.liftH $ trigger $ H.action (UpdateFilter text)
      pure next
    UpdateFilter text next → do
      H.modify (_ { filterText = text, items = L.Nil, lastLoadParams = Nothing })
      load
      pure next
    HandleScroll el next → do
      remain ← H.fromEff do
        let ul = DOM.htmlElementToElement el
        height <- DOM.clientHeight ul
        scrollTop <- DOM.scrollTop ul
        scrollHeight <- DOM.scrollHeight ul
        pure (scrollHeight - height - scrollTop)
      when (remain < 20.0) load
      pure next

  load ∷ DSL a i s f Unit
  load = do
    { filterText, nextOffset, lastLoadParams, tick } ← H.get
    let
      shouldLoad = case lastLoadParams of
        Nothing → true
        Just { filter, offset } → filter /= filterText || offset /= nextOffset
    when shouldLoad do
      let
        currentTick = tick + 1
        params = { path: colPath, filter: filterText, offset: nextOffset }
      H.modify (_ { state = Loading, lastLoadParams = Just params, tick = currentTick })
      result ← H.liftH (H.liftH (ispec.load params))
      postLoadTick ← H.gets _.tick
      when (currentTick == postLoadTick) $
        H.modify \st' → st'
          { items = st'.items <> result.items
          , nextOffset = result.nextOffset
          , state = Loaded
          }
