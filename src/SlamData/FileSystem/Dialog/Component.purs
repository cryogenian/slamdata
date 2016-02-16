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

import Control.Alt ((<|>))
import Control.Bind (join)

import Data.Array (singleton)
import Data.Either (Either())
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct)
import Data.Maybe (Maybe(..), isNothing, maybe, fromMaybe, isJust)
import Data.Path.Pathy (printPath)

import Halogen.Component
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>), prjQuery, prjSlot)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Query (request, set)
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Error.Component as Error
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Dialog.Share.Component as Share
import SlamData.FileSystem.Dialog.SQLMount.Component as SQLMount
import SlamData.Effects (Slam())
import SlamData.FileSystem.Resource (Resource())
import SlamData.Render.Common (fadeWhen)
import Utils.Path (DirPath(), DirPath())


data Dialog
  = Error String
  | Share String
  | Rename Resource
  | Mount DirPath
  | Download Resource
  | SQLMount DirPath

type State = Maybe Dialog

initialState :: State
initialState = Nothing

data Query a
  = Dismiss a
  | Show Dialog a

type ChildState =
  Either SQLMount.StateP
  (Either Mount.State
   (Either Download.State
    (Either Error.State
     (Either Share.State
      Rename.State))))

type ChildQuery =
  Coproduct SQLMount.QueryP
  (Coproduct Mount.Query
   (Coproduct Download.Query
    (Coproduct Error.Query
     (Coproduct Share.Query
      Rename.Query))))

newtype MountSlot = MountSlot DirPath
newtype SQLMountSlot = SQLMountSlot DirPath
type DownloadSlot = Resource
type ErrorSlot = String
type ShareSlot = String
type RenameSlot = Resource


instance eqMountSlot :: Eq MountSlot where
  eq (MountSlot p) (MountSlot p') = on eq printPath p p'
instance ordMountSlot :: Ord MountSlot where
  compare (MountSlot p) (MountSlot p') = on compare printPath p p'

instance eqSQLMountSlot :: Eq SQLMountSlot where
  eq (SQLMountSlot p) (SQLMountSlot p') = on eq printPath p p'
instance ordSQLMountSlot :: Ord SQLMountSlot where
  compare (SQLMountSlot p) (SQLMountSlot p') = on compare printPath p p'

type ChildSlot =
  Either SQLMountSlot
  (Either MountSlot
   (Either DownloadSlot
    (Either ErrorSlot
     (Either ShareSlot
      RenameSlot))))

cpSQLMount :: ChildPath
              SQLMount.StateP ChildState
              SQLMount.QueryP ChildQuery
              SQLMountSlot ChildSlot
cpSQLMount = cpL

cpMount :: ChildPath
           Mount.State ChildState
           Mount.Query ChildQuery
           MountSlot ChildSlot
cpMount = cpR :> cpL

cpDownload :: ChildPath
              Download.State ChildState
              Download.Query ChildQuery
              DownloadSlot ChildSlot
cpDownload = cpR :> cpR :> cpL

cpError :: ChildPath
           Error.State ChildState
           Error.Query ChildQuery
           ErrorSlot ChildSlot
cpError = cpR :> cpR :> cpR :> cpL

cpShare :: ChildPath
           Share.State ChildState
           Share.Query ChildQuery
           ShareSlot ChildSlot
cpShare = cpR :> cpR :> cpR :> cpR :> cpL

cpRename :: ChildPath
            Rename.State ChildState
            Rename.Query ChildQuery
            RenameSlot ChildSlot
cpRename = cpR :> cpR :> cpR :> cpR :> cpR

type StateP = InstalledState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type DialogDSL = ParentDSL State ChildState Query ChildQuery Slam ChildSlot


comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: RenderParent State ChildState Query ChildQuery Slam ChildSlot
render state =
  H.div [ P.classes ([B.modal] <> fadeWhen (isNothing state))
        , E.onClick (E.input_ Dismiss)
        ]
  $ maybe [ ] (singleton <<< dialog) state
  where
  dialog (Error str) =
    H.slot' cpError str \_ ->
      { component: Error.comp
      , initialState: Error.State str
      }
  dialog (Share str) =
    H.slot' cpShare str \_ ->
      { component: Share.comp
      , initialState: Share.State str
      }
  dialog (Rename res) =
    H.slot' cpRename res \_ ->
      { component: Rename.comp
      , initialState: Rename.initialState res
      }
  dialog (Download res) =
    H.slot' cpDownload res \_ ->
      { component: Download.comp
      , initialState: Download.initialState res
      }
  dialog (Mount path) =
    H.slot' cpMount (MountSlot path) \_ ->
      { component: Mount.comp
      , initialState: Mount.initialState path
      }
  dialog (SQLMount path) =
    H.slot' cpSQLMount (SQLMountSlot path) \_ ->
      { component: SQLMount.comp
      , initialState: installedState $ SQLMount.initialState path
      }


eval :: EvalParent Query State ChildState Query ChildQuery Slam ChildSlot
eval (Dismiss next) = set Nothing $> next
eval (Show d next) = set (Just d) $> next

-- | Children can only close dialog. Other peeking in `FileSystem`
peek :: forall a. ChildF ChildSlot ChildQuery a -> DialogDSL Unit
peek (ChildF slot query) =
  fromMaybe (pure unit)
  $   (errorPeek <$> prjQuery cpError query <*> prjSlot cpError slot)
  <|> (sharePeek <$> prjQuery cpShare query <*> prjSlot cpShare slot)
  <|> (renamePeek <$> prjQuery cpRename query <*> prjSlot cpRename slot)
  <|> (mountPeek <$> prjQuery cpMount query <*> prjSlot cpMount slot)
  <|> (downloadPeek <$> prjQuery cpDownload query <*> prjSlot cpDownload slot)
  <|> (sqlMountPeek <$> prjQuery cpSQLMount query <*> prjSlot cpSQLMount slot)

errorPeek :: forall a. Error.Query a -> ErrorSlot -> DialogDSL Unit
errorPeek (Error.Dismiss _) _ = set Nothing

sharePeek :: forall a. Share.Query a -> ShareSlot -> DialogDSL Unit
sharePeek (Share.Dismiss _) _ = set Nothing
sharePeek _ _ = pure unit

renamePeek :: forall a. Rename.Query a -> RenameSlot -> DialogDSL Unit
renamePeek (Rename.Dismiss _) _ = set Nothing
renamePeek _ _ = pure unit

mountPeek :: forall a. Mount.Query a -> MountSlot -> DialogDSL Unit
mountPeek (Mount.Dismiss _) _ = set Nothing
mountPeek (Mount.Save _) slot = do
  saved <- query' cpMount slot (request Mount.GetSaved)
  if isJust $ join $ saved
    then set Nothing
    else pure unit
mountPeek _ _ = pure unit

downloadPeek :: forall a. Download.Query a -> DownloadSlot -> DialogDSL Unit
downloadPeek (Download.Dismiss _) _ = set Nothing
downloadPeek _ _ = pure unit

sqlMountPeek :: forall a. SQLMount.QueryP a -> SQLMountSlot -> DialogDSL Unit
sqlMountPeek q s = coproduct (sqlMountPeekExact s) (const $ pure unit) q

sqlMountPeekExact :: forall a. SQLMountSlot -> SQLMount.Query a -> DialogDSL Unit
sqlMountPeekExact _ (SQLMount.Dismiss _) = set Nothing
sqlMountPeekExact _ (SQLMount.Success _ _) = set Nothing
sqlMountPeekExact _ _ = pure unit
