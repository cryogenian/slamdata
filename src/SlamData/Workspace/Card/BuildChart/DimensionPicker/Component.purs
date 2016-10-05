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

module SlamData.Workspace.Card.BuildChart.DimensionPicker.Component where

import SlamData.Prelude

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree as Cofree

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)

data Query s a
  = Choose (List Int) a
  | Dismiss a
  | Confirm (List s) a

type State s =
  { values ∷ Cofree List s
  , selection ∷ Maybe (List Int)
  , cursor ∷ List Int
  }

initialState ∷ ∀ s. Cofree List s → State s
initialState values =
  { values
  , selection: Nothing
  , cursor: Nil
  }

type ChildState s = Void

type ChildQuery s = Const Void

type ChildSlot = Unit

type StateP s = H.ParentState (State s) (ChildState s) (Query s) (ChildQuery s) Slam ChildSlot

type QueryP s = H.ParentQuery (Query s) (ChildQuery s) ChildSlot

type HTML s = H.ParentHTML (ChildState s) (Query s) (ChildQuery s) Slam ChildSlot

type DSL s = H.ParentDSL (State s) (ChildState s) (Query s) (ChildQuery s) Slam ChildSlot

type PickerOptions s =
  { label  ∷ s → String
  , render ∷ s → HTML s
  , weight ∷ s → Number
  , title  ∷ String
  }

picker
  ∷ ∀ s
  . PickerOptions s
  → H.Component (StateP s) (QueryP s) Slam
picker opts = H.parentComponent { render, eval, peek: Nothing }
  where
  render ∷ State s → HTML s
  render st =
    let cof = snd <$> getCursor st.cursor st.values in
    HH.div
      [ HP.classes [ HH.className "sd-dimension-picker" ] ]
      [ HH.div
          [ HP.classes [ HH.className "sd-dimension-picker-title" ] ]
          [ HH.h1_ [ HH.text opts.title ]
          , HH.button
              [ HP.classes [ HH.className "sd-dismiss-button" ]
              , HP.title "Dismiss"
              , ARIA.label "Dismiss"
              , HE.onClick (HE.input_ Dismiss)
              ]
              [ HH.text "×"]
          ]
      , HH.div
          [ HP.classes [ HH.className "sd-dimension-picker-content" ] ]
          [ HH.ul_ (renderedValues cof) ]
      , HH.div
          [ HP.classes [ HH.className "sd-dimension-picker-toolbar" ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnDefault ]
              , ARIA.label "Dismiss"
              , HE.onClick (HE.input_ Dismiss)
              ]
              [ HH.text "Dismiss" ]
          , HH.button
              ([ HP.classes [ B.btn, B.btnPrimary ]
              , ARIA.label ""
              ] <>
                case flip getCursor st.values =<< st.selection of
                  Just (p × cof') → [ HE.onClick (HE.input_ (Confirm (List.snoc p (Cofree.head cof')))) ]
                  Nothing → [ HP.disabled true ])
              [ HH.text "Confirm" ]
          ]
      ]
    where
    renderedValues Nothing = []
    renderedValues (Just parent) =
      let
        values =
          Array.mapWithIndex
            (\ix val → renderValue (ix : st.cursor) (opts.label val) (opts.render val))
            (Array.fromFoldable
              (Cofree.head <$> Cofree.tail parent))
      in
        maybe [] (pure ∘ renderBack) (List.tail st.cursor) <> values

    renderValue cursor label html =
      HH.li
        [ HP.classes (HH.className "selected" <$ guard (Just cursor ≡ st.selection)) ]
        [ HH.button
            [ HP.title label
            , ARIA.label label
            , HE.onClick (HE.input_ (Choose cursor))
            ]
            [ html ]
        ]

    renderBack back =
      renderValue back "Back" (HH.text "Back")

  eval ∷ Query s ~> DSL s
  eval = case _ of
    Choose cursor next → do
      st ← H.get
      for_ (getCursor cursor st.values) \(p × cof) →
        if List.null (Cofree.tail cof)
          then H.modify _ { selection = Just cursor }
          else H.modify _ { cursor = cursor, selection = Nothing }
      pure next
    Dismiss next →
      pure next
    Confirm _ next →
      pure next

getCursor
  ∷ ∀ a
  . List Int
  → Cofree List a
  → Maybe (List a × Cofree List a)
getCursor cursor = go Nil (List.reverse cursor)
  where
  go p Nil cof = Just (List.reverse p × cof)
  go p (i : is) cof = go (Cofree.head cof : p) is =<< List.index (Cofree.tail cof) i
