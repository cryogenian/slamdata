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

import Prelude

import Control.UI.Browser (newTab)

import Data.Array (sort)
import Data.Either (Either(..), either)
import Data.Functor.Eff (liftEff)
import Data.Lens ((.~), (%~), (<>~), _Left, _Right)
import Data.Maybe (isJust, maybe)
import Data.String as Str

import Halogen

import SlamData.Download.Model as D
import SlamData.FileSystem.Dialog.Download.Component.Query
import SlamData.FileSystem.Dialog.Download.Component.Render
import SlamData.FileSystem.Dialog.Download.Component.State
import SlamData.Effects (Slam())
import SlamData.FileSystem.Resource (Resource(..), resourceName)

import Utils.Path (parseAnyPath)

comp :: Component State Query Slam
comp = component render eval

eval :: Eval Query State Query Slam
eval (SourceTyped s next) = do
  modify (_source .~ maybe (Left s) (Right <<< either File Directory)
          (parseAnyPath s))
  modify validate
  pure next
eval (ToggleList next) = do
  modify (_showSourcesList %~ not)
  modify validate
  pure next
eval (SourceClicked r next) = do
  modify $ (_showSourcesList .~ false)
       <<< (_targetName .~ (Right $ resourceName r))
       <<< (_source .~ Right r)
  modify validate
  pure next
eval (TargetTyped s next) = do
  modify (_targetName .~ (if isJust $ Str.indexOf "/" s then Left else Right) s)
  modify validate
  pure next
eval (ToggleCompress next) = do
  modify (_compress %~ not)
  modify validate
  pure next
eval (SetOutput ty next) = do
  modify (_options %~ case ty of
             D.CSV -> Left <<< either id (const D.initialCSVOptions)
             D.JSON -> Right <<< either (const D.initialJSONOptions) id
         )
  modify validate
  pure next
eval (ModifyCSVOpts fn next) = do
  modify (_options <<< _Left %~ fn)
  modify validate
  pure next
eval (ModifyJSONOpts fn next) = do
  modify (_options <<< _Right %~ fn)
  modify validate
  pure next
eval (NewTab url next) = do
  liftEff $ newTab url
  pure next
eval (Dismiss next) =
  pure next
eval (SetSources srcs next) = do
  modify (_sources .~ srcs)
  modify (_sources %~ sort)
  pure next
eval (AddSources srcs next) = do
  modify (_sources <>~ srcs)
  modify (_sources %~ sort)
  pure next
