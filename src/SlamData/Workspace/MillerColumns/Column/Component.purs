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
  ( ColumnOptions
  , InitialItemState(..)
  , ItemHTML
  , component
  , module SlamData.Workspace.MillerColumns.Column.Component.Query
  , module SlamData.Workspace.MillerColumns.Column.Component.State
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.List ((:))
import Data.List as L

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Render.Common (clearFieldIcon)
import SlamData.Workspace.MillerColumns.Column.Component.Query (ItemQuery(..), ItemQuery', Query(..), Query')
import SlamData.Workspace.MillerColumns.Column.Component.State (ColumnState(..), State, State', initialState)

type ColumnOptions a i s f =
  { render
      ∷ L.List i
      → a
      → InitialItemState
      → { component :: H.Component s (ItemQuery' f) Slam
         , initialState :: s
         }
  , label ∷ a → String
  , load ∷ L.List i → Slam (Maybe (L.List a))
  , isLeaf ∷ L.List i → Boolean
  , id ∷ a → i
  }

data InitialItemState = Selected | Deselected

derive instance eqInitialItemState :: Eq InitialItemState
derive instance ordInitialItemState :: Ord InitialItemState

type HTML i s f = H.ParentHTML s (Query i) (ItemQuery' f) Slam i
type DSL a i s f = H.ParentDSL (State a i) s (Query i) (ItemQuery' f) Slam i

type ItemHTML = H.ComponentHTML (Const Void)

type RenderRec i s f = { path ∷ L.List i, html ∷ Array (HTML i s f) }

component
  ∷ ∀ a i s f
  . Ord i
  ⇒ ColumnOptions a i s f
  → L.List i
  → H.Component (State' a i s f) (Query' i f) Slam
component ispec colPath =
  H.lifecycleParentComponent
    { render
    , eval
    , initializer: Just (H.action Load)
    , finalizer: Nothing
    , peek: Nothing
    }
  where

  render ∷ State a i → HTML i s f
  render { items, state, selected } =
    let
      listItems = A.fromFoldable (renderItem selected <$> items)
    in
      HH.div_
        [ renderSelected selected items
        , HH.ul_
            $ listItems
            <> (guard (state == Loading) $> loadIndicator)
        ]

  loadIndicator ∷ HTML i s f
  loadIndicator =
    HH.li
      [ HP.class_ (HH.className "sd-miller-column-loading") ]
      [ HH.span_ [ HH.text "Loading..." ] ]

  renderSelected ∷ Maybe i → L.List a → HTML i s f
  renderSelected sel items =
    case L.find (\x → sel == Just (ispec.id x)) items of
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
            , clearFieldIcon deselLabel
            ]

  renderItem ∷ Maybe i → a → HTML i s f
  renderItem selected item =
    let
      itemId = ispec.id item
    in
      HH.slot itemId \_ →
        ispec.render
          (itemId : colPath)
          item
          (if Just (ispec.id item) == selected then Selected else Deselected)

  eval ∷ Query i ~> DSL a i s f
  eval = case _ of
    Load next → do
      H.modify (\st → st { state = Loading })
      H.liftH (H.liftH (ispec.load colPath)) >>= traverse_ \items -> do
        H.modify (_ { items = items, state = Loaded })
      pure next
    SetSelection selected next → do
      H.gets _.selected >>= traverse \selId →
        H.query selId $ left $ H.action $ ToggleHighlight false
      H.modify (_ { selected = selected })
      for_ selected \selId ->
        H.query selId $ left $ H.action $ ToggleHighlight true
      pure next
    Deselect next → do
      pure next
