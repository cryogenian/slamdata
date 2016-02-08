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

module SlamData.FileSystem.Dialog.Component where

import Prelude

import Data.Array (singleton)
import Data.Either.Nested (Either5())
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct)
import Data.Functor.Coproduct.Nested (Coproduct5(), coproduct5)
import Data.Maybe (Maybe(..), isNothing, maybe)

import Halogen.Component
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Query (set)
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Error.Component as Error
import SlamData.Effects (Slam())
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Mount.Component (MountSettings())
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Dialog.Share.Component as Share
import SlamData.FileSystem.Resource (Resource())
import SlamData.Render.Common (fadeWhen)

import Utils.Path (DirPath())

data Dialog
  = Error String
  | Share String
  | Rename Resource
  | Mount DirPath String (Maybe MountSettings)
  | Download Resource

type State = Maybe Dialog

initialState :: State
initialState = Nothing

data Query a
  = Dismiss a
  | Show Dialog a

type ChildState =
  Either5
    Mount.StateP
    Download.State
    Error.State
    Share.State
    Rename.State

type ChildQuery =
  Coproduct5
    Mount.QueryP
    Download.Query
    Error.Query
    Share.Query
    Rename.Query

type MountSlot = Unit
type DownloadSlot = Unit
type ErrorSlot = Unit
type ShareSlot = Unit
type RenameSlot = Unit

type ChildSlot =
  Either5
    MountSlot
    DownloadSlot
    ErrorSlot
    ShareSlot
    RenameSlot

cpMount
  :: ChildPath
       Mount.StateP ChildState
       Mount.QueryP ChildQuery
       MountSlot ChildSlot
cpMount = cpL :> cpL :> cpL :> cpL

cpDownload
  :: ChildPath
       Download.State ChildState
       Download.Query ChildQuery
       DownloadSlot ChildSlot
cpDownload = cpL :> cpL :> cpL :> cpR

cpError
  :: ChildPath
       Error.State ChildState
       Error.Query ChildQuery
       ErrorSlot ChildSlot
cpError = cpL :> cpL :> cpR

cpShare
  :: ChildPath
       Share.State ChildState
       Share.Query ChildQuery
       ShareSlot ChildSlot
cpShare = cpL :> cpR

cpRename
  :: ChildPath
       Rename.State ChildState
       Rename.Query ChildQuery
       RenameSlot ChildSlot
cpRename = cpR

type StateP = InstalledState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type DialogDSL = ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval (peek <<< runChildF)

render :: RenderParent State ChildState Query ChildQuery Slam ChildSlot
render state =
  H.div
    [ P.classes ([B.modal] <> fadeWhen (isNothing state))
    , E.onClick (E.input_ Dismiss)
    ]
    $ maybe [] (singleton <<< dialog) state
  where
  dialog (Error str) =
    H.slot' cpError unit \_ ->
      { component: Error.comp
      , initialState: Error.State str
      }
  dialog (Share str) =
    H.slot' cpShare unit \_ ->
      { component: Share.comp
      , initialState: Share.State str
      }
  dialog (Rename res) =
    H.slot' cpRename unit \_ ->
      { component: Rename.comp
      , initialState: Rename.initialState res
      }
  dialog (Download res) =
    H.slot' cpDownload unit \_ ->
      { component: Download.comp
      , initialState: Download.initialState res
      }
  dialog (Mount parent name settings) =
    H.slot' cpMount unit \_ ->
      { component: Mount.comp
      , initialState: installedState (Mount.initialState parent name settings)
      }

eval :: EvalParent Query State ChildState Query ChildQuery Slam ChildSlot
eval (Dismiss next) = set Nothing $> next
eval (Show d next) = set (Just d) $> next

-- | Children can only close dialog. Other peeking in `FileSystem`
peek :: forall a. ChildQuery a -> DialogDSL Unit
peek = coproduct5 mountPeek downloadPeek errorPeek sharePeek renamePeek

errorPeek :: forall a. Error.Query a -> DialogDSL Unit
errorPeek (Error.Dismiss _) = set Nothing

sharePeek :: forall a. Share.Query a -> DialogDSL Unit
sharePeek (Share.Dismiss _) = set Nothing
sharePeek _ = pure unit

renamePeek :: forall a. Rename.Query a -> DialogDSL Unit
renamePeek (Rename.Dismiss _) = set Nothing
renamePeek _ = pure unit

mountPeek :: forall a. Mount.QueryP a -> DialogDSL Unit
mountPeek = coproduct go (const (pure unit))
  where
  go (Mount.Dismiss _) = set Nothing
  go _ = pure unit

downloadPeek :: forall a. Download.Query a -> DialogDSL Unit
downloadPeek (Download.Dismiss _) = set Nothing
downloadPeek _ = pure unit
