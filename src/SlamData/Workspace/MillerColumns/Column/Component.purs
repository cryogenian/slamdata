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

import Control.Monad.Fork.Class (fork)

import Data.Array as A
import Data.List as L
import Data.Time.Duration (Milliseconds(..))

import DOM.Classy.Event (currentTarget) as DOM
import DOM.Classy.Node (fromNode) as DOM
import DOM.Classy.Element (scrollTop, scrollHeight, clientHeight) as DOM

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Render.Common as RC
import SlamData.Workspace.MillerColumns.Column.Options (ColumnOptions, LoadParams)
import SlamData.Workspace.MillerColumns.Column.Component.Query (Message(..), Message', Query(..))
import SlamData.Workspace.MillerColumns.Column.Component.State (ColumnState(..), State, initialState)
import SlamData.Workspace.MillerColumns.Column.Component.Item as I

import Halogen.Component.Utils.Debounced (debouncedEventSource, runDebounceTrigger)

type HTML a i f o = H.ParentHTML (Query a i o) f i Slam
type DSL a i f o = H.ParentDSL (State a i o) (Query a i o) f i (Message' a i o) Slam

component
  ∷ ∀ a i f o
  . Ord i
  ⇒ ColumnOptions a i f o
  → i
  → H.Component HH.HTML (Query a i o) (Maybe a) (Message' a i o) Slam
component ispec colPath =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: HE.input SetSelection
    }
  where

  render ∷ State a i o → HTML a i f o
  render { items, state, selected, filterText } =
    let
      listItems = A.fromFoldable (renderItem selected <$> items)
    in
      HH.div_
        [ HH.div
            [ HP.class_ (HH.ClassName "sd-miller-column-filter") ]
            [ HH.div
                [ HP.class_ (HH.ClassName "sd-action-filter-icon") ]
                [ RC.searchFieldIcon ]
            , HH.input
                [ HP.class_ (HH.ClassName "sd-form-input")
                , HP.value filterText
                , HE.onValueInput (HE.input HandleFilterChange)
                ]
            , HH.button
                [ HP.type_ HP.ButtonButton
                , HE.onClick (HE.input_ (UpdateFilter ""))
                , HP.enabled (filterText /= "")
                ]
                [ RC.clearFieldIcon "Clear filter" ]
            ]
        , renderSelected selected
        , HH.ul
            [ HE.onScroll \e -> H.action <<< HandleScroll <$> DOM.fromNode (DOM.currentTarget e) ]
            $ listItems
            <> (guard (state == Loading) $> loadIndicator)
        ]

  loadIndicator ∷ HTML a i f o
  loadIndicator =
    HH.li
      [ HP.class_ (HH.ClassName "sd-miller-column-loading") ]
      [ HH.span_ [ HH.text "Loading..." ] ]

  renderSelected ∷ Maybe a → HTML a i f o
  renderSelected = case _ of
    Nothing →
      HH.div
        [ HP.class_ (HH.ClassName "sd-miller-column-selection") ]
        [ HH.span_ [ HH.text "No selection" ] ]
    Just x →
      let
        label = ispec.label x
        deselLabel = "Deselect '" <> label <> "'"
      in
        HH.div
          [ HP.classes
              [ HH.ClassName "sd-miller-column-selection"
              , HH.ClassName "selected"
              ]
          , HP.title deselLabel
          , ARIA.label deselLabel
          , HE.onClick $ HE.input_ Deselect
          ]
          [ HH.span
              [ HP.class_ (HH.ClassName "sd-miller-column-selection-label") ]
              [ HH.text label ]
          , RC.clearFieldIcon deselLabel
          ]

  renderItem ∷ Maybe a → a → HTML a i f o
  renderItem selected item =
    let
      itemId = ispec.id item
      selectedId = ispec.id <$> selected
    in
      HH.slot
        itemId
        (ispec.render itemId item)
        (if Just itemId == selectedId then I.Selected else I.Deselected)
        (HE.input (HandleMessage itemId))

  eval ∷ Query a i o ~> DSL a i f o
  eval = case _ of
    Init next → do
      trigger ← debouncedEventSource (Milliseconds 750.0)
      H.modify (_ { filterTrigger = trigger })
      fork load
      H.raise (Left Initialized)
      pure next
    SetSelection selected next → do
      H.modify (_ { selected = selected })
      pure next
    GetSelection k →
      k <$> H.gets _.selected
    Deselect next → do
      H.raise (Left Deselected)
      pure next
    HandleFilterChange text next → do
      trigger ← H.gets _.filterTrigger
      H.modify (\st → st { tick = st.tick + 1, filterText = text })
      lift $ runDebounceTrigger trigger (UpdateFilter text)
      pure next
    UpdateFilter text next → do
      H.modify (_ { filterText = text, items = L.Nil, lastLoadParams = Nothing })
      load
      pure next
    HandleScroll ul next → do
      remain ← H.liftEff do
        height <- DOM.clientHeight ul
        scrollTop <- DOM.scrollTop ul
        scrollHeight <- DOM.scrollHeight ul
        pure (scrollHeight - height - scrollTop)
      when (remain < 20.0) load
      pure next
    HandleMessage itemId msg next → do
      case msg of
        Left (I.RaisePopulate a) → do
          H.modify (_ { selected = Just a })
          H.raise $ Left $ Selected itemId a
        Right o → do
          H.raise $ Right o
      pure next

  load ∷ DSL a i f o Unit
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
      result ← lift (ispec.load params)
      postLoadTick ← H.gets _.tick
      when (currentTick == postLoadTick) $
        H.modify \st' → st'
          { items = st'.items <> result.items
          , nextOffset = result.nextOffset
          , state = Loaded
          }
