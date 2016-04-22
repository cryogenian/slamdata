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
  , QueryP
  , StateP
  , module SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
  , module SlamData.FileSystem.Dialog.Mount.SQL2.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (liftEff)

import Data.Array (filter)
import Data.Path.Pathy as Pt
import Data.StrMap as Sm

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceQuery(..), AceState, Autocomplete(..), aceConstructor)
import Ace.Types (Editor)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Effects (Slam)
import SlamData.FileSystem.Dialog.Mount.Common.Render (propList, section)
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..))
import SlamData.FileSystem.Dialog.Mount.SQL2.Component.State (State, _initialQuery, _vars, emptyVar, initialState, isEmptyVar, processState, rxEmpty, stateFromViewInfo)
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Query as API

type Query = SettingsQuery State
type StateP = H.ParentState State AceState Query AceQuery Slam Unit
type QueryP = Coproduct Query (H.ChildF Unit AceQuery)
type SQLMountDSL = H.ParentDSL State AceState Query AceQuery Slam Unit
type SQLMountHTML = H.ParentHTML AceState Query AceQuery Slam Unit

comp :: H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Nothing }

render :: State -> SQLMountHTML
render state@{ initialQuery } =
  HH.div
    [ HP.key "mount-sql2" ]
    [ section "SQL² query"
        [ HH.Slot (aceConstructor unit (aceSetup initialQuery) (Just Live)) ]
    , section "Query variables" [ propList _vars state ]
    ]

eval :: Natural Query SQLMountDSL
eval (ModifyState f next) = H.modify (processState <<< f) $> next
eval (Validate k) = do
  sql <- fromMaybe "" <$> H.query unit (H.request GetText)
  pure $ k if sql == "" then Just "Please enter a query" else Nothing
eval (Submit parent name k) = do
  sql <- fromMaybe "" <$> H.query unit (H.request GetText)
  vars <- Sm.fromFoldable <<< filter (not isEmptyVar) <$> H.gets _.vars
  let destPath = parent Pt.</> Pt.file name
      view = R.View $ destPath
      dest = R.Mount view
  result <- API.viewQuery (Left parent) destPath sql vars
  pure $ k $ map (const view) result

aceSetup :: Maybe String -> Editor -> Slam Unit
aceSetup initialQuery editor = liftEff do
  Editor.setMinLines 6 editor
  Editor.setMaxLines 10000 editor
  Editor.setAutoScrollEditorIntoView true editor
  Editor.setTheme "ace/theme/chrome" editor
  Editor.setEnableLiveAutocompletion true editor
  Editor.setEnableBasicAutocompletion true editor
  session <- Editor.getSession editor
  Session.setMode "ace/mode/sql" session
  case initialQuery of
    Just q -> void $ Editor.setValue q Nothing editor
    Nothing -> pure unit
