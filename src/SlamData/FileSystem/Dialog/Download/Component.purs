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

module SlamData.FileSystem.Dialog.Download.Component
  ( comp
  , module SlamData.FileSystem.Dialog.Download.Component.State
  , module SlamData.FileSystem.Dialog.Download.Component.Query
  ) where

import SlamData.Prelude

import Control.UI.Browser (newTab)

import Data.Array (sort, nub)
import Data.Lens ((.~), (%~), (<>~), _Left, _Right)
import Data.String as Str

import Halogen as H

import SlamData.Download.Model as D
import SlamData.Effects (Slam)
import SlamData.FileSystem.Dialog.Download.Component.Query (Query(..))
import SlamData.FileSystem.Dialog.Download.Component.Render (render)
import SlamData.FileSystem.Dialog.Download.Component.State (State, _compress, _error, _options, _showSourcesList, _source, _sources, _targetName, checkExists, initialState, validate)
import SlamData.FileSystem.Resource (Resource(..), resourceName)

import Utils.Path (parseAnyPath)

comp :: H.Component State Query Slam
comp = H.component { render, eval }

eval :: Natural Query (H.ComponentDSL State Query Slam)
eval (SourceTyped s next) = do
  H.modify (_source .~ maybe (Left s) (Right <<< either Directory File)
          (parseAnyPath s))
  H.modify validate
  pure next
eval (ToggleList next) = do
  H.modify (_showSourcesList %~ not)
  H.modify validate
  pure next
eval (SourceClicked r next) = do
  H.modify $ (_showSourcesList .~ false)
       <<< (_targetName .~ (Right $ resourceName r))
       <<< (_source .~ Right r)
  H.modify validate
  pure next
eval (TargetTyped s next) = do
  H.modify (_targetName .~ (if isJust $ Str.indexOf "/" s then Left else Right) s)
  H.modify validate
  pure next
eval (ToggleCompress next) = do
  H.modify (_compress %~ not)
  H.modify validate
  pure next
eval (SetOutput ty next) = do
  H.modify
    $ _options
    %~ case ty of
        D.CSV -> Left <<< either id (const D.initialCSVOptions)
        D.JSON -> Right <<< either (const D.initialJSONOptions) id
  H.modify validate
  pure next
eval (ModifyCSVOpts fn next) = do
  H.modify (_options <<< _Left %~ fn)
  H.modify validate
  pure next
eval (ModifyJSONOpts fn next) = do
  H.modify (_options <<< _Right %~ fn)
  H.modify validate
  pure next
eval (NewTab url next) = do
  H.fromEff $ newTab url
  pure next
eval (Dismiss next) =
  pure next
eval (SetSources srcs next) = do
  H.modify (_sources .~ srcs)
  H.modify (_sources %~ sort ∘ nub)
  pure next
eval (AddSources srcs next) = do
  H.modify (_sources <>~ srcs)
  H.modify (_sources %~ sort ∘ nub)
  pure next
