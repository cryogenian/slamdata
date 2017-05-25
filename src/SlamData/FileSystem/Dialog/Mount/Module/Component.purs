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

module SlamData.FileSystem.Dialog.Mount.Module.Component where

import SlamData.Prelude

import Data.Path.Pathy as Pt

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceQuery(..), Autocomplete(..), aceComponent)
import Ace.Types (Editor)

import Halogen as H
import Halogen.HTML as HH

import SlamData.Monad (Slam)
import SlamData.FileSystem.Dialog.Mount.Common.Render (section)
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..), SettingsMessage(..))
import SlamData.FileSystem.Dialog.Mount.Module.Component.State as MCS
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Query as API

type Query = SettingsQuery MCS.State

comp ∷ MCS.State → H.Component HH.HTML Query Unit SettingsMessage Slam
comp initialState =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ MCS.State → H.ParentHTML Query AceQuery Unit Slam
render { initialValue } =
  HH.div_
    [ section "SQL² module"
        [ HH.slot
            unit
            (aceComponent (aceSetup initialValue) (Just Live))
            unit
            (const (Just (H.action (ModifyState id))))
        ]
    ]

eval ∷ Query ~> H.ParentDSL MCS.State Query AceQuery Unit SettingsMessage Slam
eval = case _ of
  ModifyState f next → do
    H.modify f
    H.raise Modified
    pure next
  Validate k → do
    sql ← fromMaybe "" <$> H.query unit (H.request GetText)
    pure $ k if sql == "" then Just "Please provide the module's functions" else Nothing
  Submit parent name k → do
    sql ← fromMaybe "" <$> H.query unit (H.request GetText)
    let destPath = parent Pt.</> Pt.dir name
        view = R.Module $ destPath
        dest = R.Mount view
    result ← API.mountModule' destPath sql
    pure $ k $ map (const view) result

aceSetup ∷ Maybe String → Editor → Slam Unit
aceSetup initialQuery editor = H.liftEff do
  Editor.setMinLines 6 editor
  Editor.setMaxLines 10000 editor
  Editor.setAutoScrollEditorIntoView true editor
  Editor.setTheme "ace/theme/chrome" editor
  Editor.setEnableLiveAutocompletion true editor
  Editor.setEnableBasicAutocompletion true editor
  session ← Editor.getSession editor
  Session.setMode "ace/mode/sql" session
  case initialQuery of
    Just q → void $ Editor.setValue q Nothing editor
    Nothing → pure unit
