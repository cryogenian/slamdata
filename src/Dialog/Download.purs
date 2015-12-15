{-
Copyright 2015 SlamData, Inc.

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

module Dialog.Download
  ( comp
  , module Dialog.Download.State
  , module Dialog.Download.Query
  ) where

import Prelude

import Control.UI.Browser (newTab)

import Data.Array (sort)
import Data.Either (Either(..), either)
import Data.Lens ((.~), (%~), (<>~), _Left, _Right)
import Data.Maybe (isJust, maybe)
import Data.String as Str

import Halogen

import Dialog.Common (Slam())
import Dialog.Download.Query
import Dialog.Download.Render
import Dialog.Download.State
import Model.Resource (Resource(..), resourceName)
import Utils.Path (parseAnyPath)

comp :: forall e. Component State Query (Slam e)
comp = component render eval

eval :: forall e. Eval Query State Query (Slam e)
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
             CSV -> Left <<< either id (const initialCSVOptions)
             JSON -> Right <<< either (const initialJSONOptions) id
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
  liftEff' $ newTab url
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
