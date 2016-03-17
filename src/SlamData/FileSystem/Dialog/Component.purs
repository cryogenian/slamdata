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

import SlamData.Prelude

import Data.Array (singleton)
import Data.Either.Nested (Either6())
import Data.Functor.Coproduct.Nested (Coproduct6(), coproduct6)

import Halogen as H
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Error.Component as Error
import SlamData.Effects (Slam())
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Mount.Component (MountSettings())
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Dialog.Share.Component as Share
import SlamData.FileSystem.Dialog.Permissions.Component as Perms
import SlamData.FileSystem.Resource (Resource())
import SlamData.Render.Common (fadeWhen)

import Utils.Path (DirPath())

data Dialog
  = Error String
  | Share String
  | Rename Resource
  | Mount DirPath String (Maybe MountSettings)
  | Download Resource
  | Permissions Resource

type State = Maybe Dialog

initialState :: State
initialState = Nothing

data Query a
  = Dismiss a
  | Show Dialog a

type ChildState =
  Either6
    Mount.StateP
    Download.State
    Error.State
    Share.State
    Rename.State
    Perms.StateP

type ChildQuery =
  Coproduct6
    Mount.QueryP
    Download.Query
    Error.Query
    Share.Query
    Rename.Query
    Perms.QueryP

type MountSlot = Unit
type DownloadSlot = Unit
type ErrorSlot = Unit
type ShareSlot = Unit
type RenameSlot = Unit
type PermsSlot = Unit

type ChildSlot =
  Either6
    MountSlot
    DownloadSlot
    ErrorSlot
    ShareSlot
    RenameSlot
    PermsSlot

cpMount
  :: ChildPath
       Mount.StateP ChildState
       Mount.QueryP ChildQuery
       MountSlot ChildSlot
cpMount = cpL :> cpL :> cpL :> cpL :> cpL

cpDownload
  :: ChildPath
       Download.State ChildState
       Download.Query ChildQuery
       DownloadSlot ChildSlot
cpDownload = cpL :> cpL :> cpL :> cpL :> cpR

cpError
  :: ChildPath
       Error.State ChildState
       Error.Query ChildQuery
       ErrorSlot ChildSlot
cpError = cpL :> cpL :> cpL :> cpR

cpShare
  :: ChildPath
       Share.State ChildState
       Share.Query ChildQuery
       ShareSlot ChildSlot
cpShare = cpL :> cpL :> cpR

cpRename
  :: ChildPath
       Rename.State ChildState
       Rename.Query ChildQuery
       RenameSlot ChildSlot
cpRename = cpL :> cpR

cpPerms
  :: ChildPath
       Perms.StateP ChildState
       Perms.QueryP ChildQuery
       PermsSlot ChildSlot
cpPerms = cpR

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)
type DialogDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp :: H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just (peek <<< H.runChildF) }

render :: State -> H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
render state =
  HH.div
    [ HP.classes ([B.modal] <> fadeWhen (isNothing state))
    , HE.onMouseDown (HE.input_ Dismiss)
    ]
    $ maybe [] (singleton <<< dialog) state
  where
  dialog (Error str) =
    HH.slot' cpError unit \_ ->
      { component: Error.comp
      , initialState: Error.State str
      }
  dialog (Share str) =
    HH.slot' cpShare unit \_ ->
      { component: Share.comp
      , initialState: Share.State str
      }
  dialog (Rename res) =
    HH.slot' cpRename unit \_ ->
      { component: Rename.comp
      , initialState: Rename.initialState res
      }
  dialog (Download res) =
    HH.slot' cpDownload unit \_ ->
      { component: Download.comp
      , initialState: Download.initialState res
      }
  dialog (Mount parent name settings) =
    HH.slot' cpMount unit \_ ->
      { component: Mount.comp
      , initialState: H.parentState (Mount.initialState parent name settings)
      }
  dialog (Permissions res) =
    HH.slot' cpPerms unit \_ ->
      { component: Perms.comp
      , initialState: H.parentState $ Perms.initialState res
      }

eval :: Natural Query (H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot)
eval (Dismiss next) = H.set Nothing $> next
eval (Show d next) = H.set (Just d) $> next

-- | Children can only close dialog. Other peeking in `FileSystem`
peek :: forall a. ChildQuery a -> DialogDSL Unit
peek =
  coproduct6
    mountPeek
    downloadPeek
    errorPeek
    sharePeek
    renamePeek
    permsPeek

errorPeek :: forall a. Error.Query a -> DialogDSL Unit
errorPeek (Error.Dismiss _) = H.set Nothing

sharePeek :: forall a. Share.Query a -> DialogDSL Unit
sharePeek (Share.Dismiss _) = H.set Nothing
sharePeek _ = pure unit

renamePeek :: forall a. Rename.Query a -> DialogDSL Unit
renamePeek (Rename.Dismiss _) = H.set Nothing
renamePeek _ = pure unit

mountPeek :: forall a. Mount.QueryP a -> DialogDSL Unit
mountPeek = coproduct go (const (pure unit))
  where
  go (Mount.Dismiss _) = H.set Nothing
  go _ = pure unit

downloadPeek :: forall a. Download.Query a -> DialogDSL Unit
downloadPeek (Download.Dismiss _) = H.set Nothing
downloadPeek _ = pure unit

permsPeek :: forall a. Perms.QueryP a -> DialogDSL Unit
permsPeek = coproduct go (const (pure unit))
  where
  go (Perms.Dismiss _) = H.set Nothing
  go _ = pure unit
