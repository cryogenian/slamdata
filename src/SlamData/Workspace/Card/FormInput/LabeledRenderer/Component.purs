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

module SlamData.Workspace.Card.FormInput.LabeledRenderer.Component where

import SlamData.Prelude

import Data.Argonaut (JCursor(..))
import Data.Array as Arr
import Data.List as List
import Data.Set as Set
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as Cp
import Halogen.Component.Utils (sendAfter, raise)

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.Port (SetupLabeledFormInputPort)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))
import SlamData.Workspace.Card.FormInput.LabeledRenderer.Model as M

type State =
  { formInputType ∷ FormInputType
  , selected ∷ Set.Set Sem.Semantics
  , valueLabelMap ∷ Map.Map Sem.Semantics (Maybe String)
  , label ∷ Maybe String
  , cursor ∷ JCursor
  }

initialState ∷ State
initialState =
  { formInputType: Dropdown
  , selected: Set.empty
  , valueLabelMap: Map.empty
  , label: Nothing
  , cursor: JCursorTop
  }

data Query a
  = Setup SetupLabeledFormInputPort a
  | ItemSelected Sem.Semantics a
  | Load M.Model a
  | Save (M.Model → a)
  | Updated a

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state =
  HH.form [ Cp.nonSubmit ]
    $ foldMap (\n → [ HH.h3_ [ HH.text n ] ]) state.label
    ⊕ case state.formInputType of
      Dropdown → renderDropdown state
      Checkbox → renderCheckbox state
      Radio → renderRadio state
      _ → [ ]

renderDropdown ∷ State → Array HTML
renderDropdown state =
  [ HH.select
      [ HP.classes [ B.formControl ]
      , HE.onSelectedIndexChange \ix →
         pure (Arr.index optionList ix <#> fst ⋙ flip ItemSelected unit)
      ]
      $ map renderOption optionList
  ]
  where
  optionList ∷ Array (Sem.Semantics × Maybe String)
  optionList = Map.toUnfoldable state.valueLabelMap

  renderOption ∷ Sem.Semantics × Maybe String → HTML
  renderOption (sem × label) =
    HH.option
      [ HP.selected $ Set.member sem state.selected ]
      [ HH.text $ fromMaybe (Sem.printSemantics sem) label ]

renderCheckbox ∷ State → Array HTML
renderCheckbox state =
  [ HH.form
     [ Cp.nonSubmit ]
     $ map renderOneInput optionList
  ]
  where
  optionList ∷ Array (Sem.Semantics × Maybe String)
  optionList = Map.toUnfoldable state.valueLabelMap

  renderOneInput ∷ Sem.Semantics × Maybe String → HTML
  renderOneInput (sem × label) =
    HH.div
      [ HP.classes [ B.checkbox ] ]
      [ HH.label_
        [ HH.input
            [ HP.inputType HP.InputCheckbox
            , HP.checked $ Set.member sem state.selected
            , HE.onValueChange (HE.input_ $ ItemSelected sem)
            ]
        , HH.text $ fromMaybe (Sem.printSemantics sem) label
        ]
      ]

renderRadio ∷ State → Array HTML
renderRadio state =
  [ HH.form
     [ Cp.nonSubmit ]
     $ map renderOneInput optionList
  ]
  where
  optionList ∷ Array (Sem.Semantics × Maybe String)
  optionList = Map.toUnfoldable state.valueLabelMap

  renderOneInput ∷ Sem.Semantics × Maybe String → HTML
  renderOneInput (sem × label) =
    HH.div
      [ HP.classes [ B.radio ]  ]
      [ HH.label_
          [ HH.input
            [ HP.inputType HP.InputRadio
            , HP.checked $ Set.member sem state.selected
            , HE.onValueChange (HE.input_ $ ItemSelected sem)
            ]
          , HH.text $ fromMaybe (Sem.printSemantics sem) label
          ]
      ]

eval ∷ Query ~> DSL
eval (Setup conf next) = do
  st ← H.get
  let
    selectedFromConf
      -- If this is checkbox and selected values field is empty then
      -- there is no sense in setting default value (and it's actually empty :) )
      | Set.isEmpty conf.selectedValues ∧ conf.formInputType ≠ Checkbox =
          foldMap Set.singleton $ List.head $ Map.keys conf.valueLabelMap
      | otherwise =
          conf.selectedValues
    selected
      -- When cursor is changed we use default selection from input
      | st.cursor ≠ conf.cursor =
          selectedFromConf
      -- Same if user didn't interact with form input
      | Set.isEmpty st.selected =
          selectedFromConf
      -- If port is the same and user already selected something we preserve selected values
      | otherwise =
          st.selected

  H.modify _
    { formInputType = conf.formInputType
    , selected = selected
    , valueLabelMap = conf.valueLabelMap
    , label = if conf.name ≡ "" then Nothing else Just conf.name
    , cursor = conf.cursor
    }

  when (st.cursor ≠ conf.cursor)
    $ void $ sendAfter (Milliseconds 200.0) $ H.action Updated
  pure next
eval (ItemSelected sem next) = do
  st ← H.get
  case st.formInputType of
    Checkbox → do
      let
        selected =
          if Set.member sem st.selected
            then Set.delete sem st.selected
            else Set.insert sem st.selected
      H.modify _{ selected = selected }
    _ → H.modify _{ selected = Set.singleton sem }
  raise $ H.action Updated
  pure next
eval (Load m next) = do
  H.modify _
    { formInputType = m.formInputType
    , selected = m.selected
    , cursor = m.cursor
    }
  pure next
eval (Save continue) = do
  st ← H.get
  pure
    $ continue
      { formInputType: st.formInputType
      , selected: st.selected
      , cursor: st.cursor
      }
eval (Updated next) =
  pure next
