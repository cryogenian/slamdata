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

module SlamData.FileSystem.Search.Component where

import SlamData.Prelude

import Data.Array as A
import Data.Lens (lens, Lens', (.~))
import Data.Path.Pathy (printPath, rootDir)
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.HTML.Events as HE
--import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Utils as HU

import SlamData.Config as Config
import SlamData.Monad (Slam)
import SlamData.Render.Common as RC
import SlamData.FileSystem.Search.Component.CSS as CSS

import Text.SlamSearch (mkQuery)

import Utils.Path (DirPath)
--import Utils.Debounced (debouncedEventSource)

type State =
  { valid ∷ Boolean
  , focused ∷ Boolean
  , value ∷ String
  , loading ∷ Boolean
  , path ∷ DirPath
  , trigger ∷ Maybe (Query Unit → Slam Unit)
  }

initialState ∷ State
initialState =
  { valid: true
  , value: ""
  , focused: false
  , loading: true
  , path: rootDir
  , trigger: Nothing
  }

_valid ∷ ∀ a r. Lens' {valid ∷ a|r} a
_valid = lens (_.valid) (_{valid = _})

_focused ∷ ∀ a r. Lens' {focused ∷ a|r} a
_focused = lens (_.focused) (_{focused = _})

_value ∷ ∀ a r. Lens' {value ∷ a|r} a
_value = lens (_.value) (_{value = _})

_loading ∷ ∀ a r. Lens' {loading ∷ a|r} a
_loading = lens (_.loading) (_{loading = _})

_timeout ∷ ∀ a r. Lens' {timeout ∷ a|r} a
_timeout = lens (_.timeout) (_{timeout = _})

_path ∷ ∀ a r. Lens' {path ∷ a|r} a
_path = lens (_.path) (_{path = _})

_trigger ∷ ∀ a r. Lens' {trigger ∷ a|r} a
_trigger = lens (_.trigger) (_{trigger = _})

data Query a
  = Focus Boolean a
  | Typed String a
  | Clear a
  | Validate a
  | Submit a
  | GetValue (String → a)
  | SetLoading Boolean a
  | SetValue String a
  | SetValid Boolean a
  | IsSearching (Boolean → a)
  | IsLoading (Boolean → a)
  | SetPath DirPath  a

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.search ] ]
    [ HH.form
        [ HE.onSubmit (HE.input_ Submit)]
        [ HH.div
            [ HP.classes searchClasses ]
            [ HH.div
                [ HP.class_ CSS.searchIcon ]
                [ RC.searchFieldIcon ]
            , HH.input
                [ HP.classes [ B.formControl ]
                , HP.value state.value
                , HE.onFocus (HE.input_ (Focus true))
                , HE.onBlur (HE.input_ (Focus false))
                , HE.onValueInput (HE.input Typed)
                , HP.title "File search field"
                , ARIA.label "File search field"
                ]
            , HH.span
                [ HP.class_
                    if state.focused
                      then CSS.searchPathActive
                      else CSS.searchPath
                ]
                [ HH.span
                    [ HP.class_ CSS.searchPathBody ]
                    [ HH.text state.value ]
                , HH.span
                    [ HP.class_
                        if state.value ≡ ""
                        then CSS.searchAffixEmpty
                        else CSS.searchAffix
                    ]
                    [ HH.text $ "path:" ⊕ printPath (state.path) ]
                ]
            , HH.button
                [ HP.class_ CSS.searchClearButton
                  -- TODO: preventDefault
--                , HE.onClick (\_ → HEH.preventDefault $> Just (H.action Clear))
                ]
                [ if state.loading
                    then RC.busyFieldIcon "Search in progress"
                    else RC.clearFieldIcon "Clear search"
                ]
            ]
        ]
    ]
  where
  searchClasses ∷ Array HH.ClassName
  searchClasses =
    [ B.inputGroup
    , CSS.searchInput
    ]
    ⊕ A.catMaybes
        [ if not state.valid then Just B.hasError else Nothing
        , if state.value ≡ "" then Just CSS.searchEmpty else Nothing
        ]

eval ∷ Query ~> DSL
eval (Focus bool next) = H.modify (_focused .~ bool) $> next
eval (Clear next) = pure next
eval (Typed str next) = do
  state ← H.get
  H.modify (_value .~ str)
  t ← case state.trigger of
    Just t' → pure t'
    Nothing → do
      t' ←
        debouncedEventSource
          H.subscribe
          (Milliseconds Config.searchTimeout)
      H.modify (_trigger .~ pure t')
      pure t'
  H.liftH $ t $ H.action Validate
  pure next
eval (Validate next) = do
  val ← H.gets _.value
  case mkQuery val of
    Left _ | val ≠ "" → H.modify (_valid .~ false)
    _ → do
      H.modify (_valid .~ true)
      HU.sendAfter (Milliseconds zero) $ H.action Submit
      pure unit
  pure next
eval (Submit next) = pure next
eval (GetValue continue) = map continue $ H.gets _.value
eval (SetLoading bool next) = H.modify (_loading .~ bool) $> next
eval (SetValue tv next) = H.modify (_value .~ tv) $> next
eval (SetValid bool next) = H.modify (_valid .~ bool) $> next
eval (IsSearching continue) = do
  state ← H.get
  pure $ continue $ (_ ≠ "") $ state.value
eval (IsLoading continue) = map continue $ H.gets _.loading
eval (SetPath p next) = H.modify (_path .~ p) $> next
