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

module SlamData.FileSystem.Dialog.Mount.SQL2.Component
  ( comp
  , Query
  , module SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
  , module SlamData.FileSystem.Dialog.Mount.SQL2.Component.State
  ) where

import SlamData.Prelude

import Data.Array (filter)
import Data.Path.Pathy as Pt
import Data.StrMap as Sm

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceQuery(..), Autocomplete(..), aceComponent)
import Ace.Types (Editor)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHEK

import SlamData.Monad (Slam)
import SlamData.FileSystem.Dialog.Mount.Common.Render (propList, section)
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..), SettingsMessage(..))
import SlamData.FileSystem.Dialog.Mount.SQL2.Component.State (State, _initialQuery, _vars, emptyVar, initialState, isEmptyVar, processState, rxEmpty, stateFromViewInfo)
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Query as API

type Query = SettingsQuery State

comp ∷ H.Component HH.HTML Query Unit SettingsMessage Slam
comp =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ParentHTML Query AceQuery Unit Slam
render state@{ initialQuery } =
  HHEK.div_
    [ "mount-sql2-query" × section "SQL² query"
        [ HH.slot
            unit
            (aceComponent (aceSetup initialQuery) (Just Live))
            unit
            (const (Just (H.action (ModifyState id))))
        ]
    , "mount-sql2-vars" × section "Query variables" [ propList _vars state ]
    ]

eval ∷ Query ~> H.ParentDSL State Query AceQuery Unit SettingsMessage Slam
eval (ModifyState f next) = do
  H.modify (processState <<< f)
  H.raise Modified
  pure next
eval (Validate k) = do
  sql ← fromMaybe "" <$> H.query unit (H.request GetText)
  pure $ k if sql == "" then Just "Please enter a query" else Nothing
eval (Submit parent name k) = do
  sql ← fromMaybe "" <$> H.query unit (H.request GetText)
  vars ← Sm.fromFoldable <<< filter (not isEmptyVar) <$> H.gets _.vars
  let destPath = parent Pt.</> Pt.file name
      view = R.View $ destPath
      dest = R.Mount view
  result ← API.viewQuery (Left parent) destPath sql vars
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
