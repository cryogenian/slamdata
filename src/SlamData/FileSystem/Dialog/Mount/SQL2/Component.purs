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
  , Query()
  , QueryP()
  , StateP()
  , module SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
  , module SlamData.FileSystem.Dialog.Mount.SQL2.Component.State
  ) where

import Prelude

import Control.Monad.Aff (attempt)

import Data.Array (filter)
import Data.Functor (($>))
import Data.Functor.Aff (liftAff)
import Data.Functor.Coproduct (Coproduct())
import Data.Functor.Eff (liftEff)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy as Pt
import Data.StrMap as Sm

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceQuery(..), AceState(), Autocomplete(..), aceConstructor)
import Ace.Types (Editor())

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Quasar.Aff as API
import Quasar.Auth as Auth

import SlamData.Effects (Slam())
import SlamData.FileSystem.Dialog.Mount.Common.Render
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
import SlamData.FileSystem.Dialog.Mount.SQL2.Component.State
import SlamData.FileSystem.Resource as R

type Query = SettingsQuery State
type StateP = InstalledState State AceState Query AceQuery Slam Unit
type QueryP = Coproduct Query (ChildF Unit AceQuery)
type SQLMountDSL = ParentDSL State AceState Query AceQuery Slam Unit
type SQLMountHTML = ParentHTML AceState Query AceQuery Slam Unit

comp :: Component StateP QueryP Slam
comp = parentComponent render eval

render :: State -> SQLMountHTML
render state@{ initialQuery } =
  H.div
    [ P.key "mount-sql2" ]
    [ section "SQL² query"
        [ H.Slot (aceConstructor unit (aceSetup initialQuery) (Just Live)) ]
    , section "Query variables" [ propList _vars state ]
    ]

eval :: Natural Query SQLMountDSL
eval (ModifyState f next) = modify (processState <<< f) $> next
eval (Validate k) = do
  sql <- fromMaybe "" <$> query unit (request GetText)
  pure $ k if sql == "" then Just "Please enter a query" else Nothing
eval (Submit parent name k) = do
  sql <- fromMaybe "" <$> query unit (request GetText)
  vars <- Sm.fromFoldable <<< filter (not isEmptyVar) <$> gets _.vars
  let res = R.Directory parent
      view = R.View $ parent Pt.</> Pt.file name
      dest = R.Mount view
  result <- liftAff $ attempt $ Auth.authed $ API.portView res dest sql vars
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
