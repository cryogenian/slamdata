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

module SlamData.FileSystem.Component
  ( module SlamData.FileSystem.Component.State
  , module SlamData.FileSystem.Component.Query
  , module SlamData.FileSystem.Component.Install
  , comp
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.UI.Browser (setLocation, locationString, clearValue)
import Control.UI.Browser.Event as Be
import Control.UI.File as Cf

import Data.Argonaut (jsonParser, jsonEmptyObject)
import Data.Array (head, last, mapMaybe, filter)
import Data.Foldable as F
import Data.Lens ((.~), preview)
import Data.MediaType.Common (textCSV, applicationJSON)
import Data.Path.Pathy (rootDir, (</>), dir, file, parentDir)
import Data.String as S
import Data.String.Regex as Rgx

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, injSlot, prjQuery, injQuery)
import Halogen.Component.Utils (subscribeToBus')
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Quasar.Data (QData(..))

import SlamData.Config as Config
import SlamData.Effects (Slam)
import SlamData.FileSystem.Breadcrumbs.Component as Breadcrumbs
import SlamData.FileSystem.Component.CSS as CSS
import SlamData.FileSystem.Component.Install (ChildQuery, ChildSlot, ChildState, QueryP, StateP, cpBreadcrumbs, cpDialog, cpListing, cpSearch, cpHeader, toDialog, toFs, toListing, toSearch)
import SlamData.FileSystem.Component.Query (Query(..))
import SlamData.FileSystem.Component.Render (sorting, toolbar)
import SlamData.FileSystem.Component.State (State, _isMount, _path, _salt, _showHiddenFiles, _sort, _version, initialState)
import SlamData.FileSystem.Dialog.Component as Dialog
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Explore.Component as Explore
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State as MongoDB
import SlamData.FileSystem.Dialog.Mount.SQL2.Component.State as SQL2
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Listing.Component as Listing
import SlamData.FileSystem.Listing.Item (Item(..), itemResource, sortItem)
import SlamData.FileSystem.Listing.Item.Component as Item
import SlamData.FileSystem.Listing.Sort (notSort)
import SlamData.FileSystem.Resource as R
import SlamData.FileSystem.Routing (browseURL)
import SlamData.FileSystem.Routing.Salt (newSalt)
import SlamData.FileSystem.Search.Component as Search
import SlamData.GlobalError as GE
import SlamData.Header.Component as Header
import SlamData.Quasar (ldJSON) as API
import SlamData.Quasar.Auth (authHeaders) as API
import SlamData.Quasar.Data (makeFile, save) as API
import SlamData.Quasar.FS (children, delete, getNewName) as API
import SlamData.Quasar.Mount (mountInfo, viewInfo) as API
import SlamData.Render.Common (content, row)
import SlamData.SignIn.Component as SignIn
import SlamData.Workspace.Action (Action(..), AccessType(..))
import SlamData.Workspace.Routing (mkWorkspaceURL)

import Utils.DOM as D
import Utils.Path (DirPath, getNameStr)

type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ Bus.BusRW GE.GlobalError → H.Component StateP QueryP Slam
comp bus =
  case Bus.split bus of
    _ × busW →
      H.lifecycleParentComponent
        { render: render busW
        , eval: eval bus
        , peek: Just (peek busW ∘ H.runChildF)
        , initializer: Just (H.action Init)
        , finalizer: Nothing
        }

render ∷ Bus.BusW GE.GlobalError → State → HTML
render bus state@{ version, sort, salt, path } =
  HH.div
    [ HP.classes [ CSS.filesystem ]
    , HE.onClick (HE.input_ DismissSignInSubmenu)
    ]
    [ HH.slot' cpHeader unit \_ →
          { component: Header.comp
          , initialState: H.parentState Header.initialState
          }

    , content
        [ HH.slot' cpSearch unit \_ →
             { component: Search.comp
             , initialState: Search.initialState
             }
        , HH.div_
            [ HH.slot' cpBreadcrumbs unit \_ →
                { component: Breadcrumbs.comp
                , initialState: Breadcrumbs.mkBreadcrumbs path sort salt
                }
            , toolbar state
            ]
        , row [ sorting state ]
        , HH.slot' cpListing unit \_ →
            { component: Listing.comp
            , initialState: H.parentState Listing.initialState
            }
        ]
    , HH.slot' cpDialog unit \_ →
        { component: Dialog.comp bus
        , initialState: H.parentState Dialog.initialState
        }
    ]

eval ∷ Bus.BusRW GE.GlobalError → Query ~> DSL
eval bus (Init next) = do
  subscribeToBus' (H.action ∘ HandleError) bus
  pure next
eval _ (Resort next) = do
  { sort, salt, path } ← H.get
  searchValue ← H.query' cpSearch unit (H.request Search.GetValue)
  H.fromEff $ setLocation $ browseURL searchValue (notSort sort) salt path
  pure next
eval _ (SetPath path next) = H.modify (_path .~ path) *> updateBreadcrumbs $> next
eval _ (SetSort sort next) = do
  H.modify (_sort .~ sort)
  updateBreadcrumbs
  resort
  pure next
eval _ (SetSalt salt next) = H.modify (_salt .~ salt) *> updateBreadcrumbs $> next
eval _ (SetIsMount isMount next) = H.modify (_isMount .~ isMount) $> next
eval _ (ShowHiddenFiles next) = do
  H.modify (_showHiddenFiles .~ true)
  queryListing $ H.action (Listing.SetIsHidden false)
  pure next
eval _ (HideHiddenFiles next) = do
  H.modify (_showHiddenFiles .~ false)
  queryListing $ H.action (Listing.SetIsHidden true)
  pure next
eval bus (Configure next) = do
  path ← H.gets _.path
  configure bus (R.Database path)
  pure next
eval _ (MakeMount next) = do
  path ← H.gets _.path
  showDialog (Dialog.Mount path "" Nothing)
  pure next
eval bus (MakeFolder next) = do
  result ← runExceptT do
    path ← lift $ H.gets _.path
    dirName ← ExceptT $ API.getNewName path Config.newFolderName
    let dirPath = path </> dir dirName
        dirRes = R.Directory dirPath
        dirItem = PhantomItem dirRes
        hiddenFile = dirPath </> file (Config.folderMark)
    lift $ queryListing $ H.action (Listing.Add dirItem)
    ExceptT $ API.save hiddenFile jsonEmptyObject
    lift $ queryListing $ H.action (Listing.Filter (_ ≠ dirItem))
    pure dirRes
  case result of
    Left err →
      case GE.fromQError err of
        Left msg →
          showDialog $ Dialog.Error
            $ "There was a problem creating the directory: " ⊕ msg
        Right ge →
          H.fromAff $ Bus.write ge bus
    Right dirRes →
      void $ queryListing $ H.action $ Listing.Add (Item dirRes)
  pure next

eval bus (MakeWorkspace next) = do
  path ← H.gets _.path
  let newWorkspaceName = Config.newWorkspaceName ⊕ "." ⊕ Config.workspaceExtension
  name ← API.getNewName path newWorkspaceName
  case name of
    Left err →
      case GE.fromQError err of
        Left msg →
          -- This error isn't strictly true as we're not actually creating the
          -- workspace here, but saying there was a problem "creating a name for the
          -- workspace" would be a little strange
          showDialog $ Dialog.Error
            $ "There was a problem creating the workspace: " ⊕ msg
        Right ge →
          H.fromAff $ Bus.write ge bus
    Right name' → do
      H.fromEff $ setLocation $ mkWorkspaceURL (path </> dir name') New
  pure next

eval _ (UploadFile el next) = do
  mbInput ← H.fromEff $ D.querySelector "input" el
  for_ mbInput \input →
    void $ H.fromEff $ Be.raiseEvent "click" input
  pure next

eval bus (FileListChanged el next) = do
  fileArr ← map Cf.fileListToArray $ (H.fromAff $ Cf.files el)
  H.fromEff $ clearValue el
  case head fileArr of
    Nothing →
      let err ∷ Slam Unit
          err = throwError $ error "empty filelist"
      in H.fromAff err
    Just f → uploadFileSelected bus f
  pure next

eval _ (Download next) = do
  path ← H.gets _.path
  download (R.Directory path)
  pure next

eval _ (SetVersion version next) = H.modify (_version .~ Just version) $> next
eval _ (DismissSignInSubmenu next) = dismissSignInSubmenu $> next
eval _ (HandleError ge next) = do
  showDialog $ Dialog.Error $ GE.print ge
  pure next

uploadFileSelected
  ∷ ∀ r
  . Bus.Bus (write ∷ Bus.Cap | r) GE.GlobalError
  → Cf.File
  → DSL Unit
uploadFileSelected bus f = do
  { path, sort, salt } ← H.get
  name ←
    H.fromEff (Cf.name f)
      <#> Rgx.replace (unsafePartial fromRight $ Rgx.regex "/" Rgx.noFlags{global=true}) ":"
      >>= API.getNewName path

  case name of
    Left err → handleError err
    Right name' → do
      reader ← H.fromEff Cf.newReaderEff
      content' ← H.fromAff $ Cf.readAsBinaryString f reader

      let fileName = path </> file name'
          res = R.File fileName
          fileItem = PhantomItem res
          ext = last (S.split "." name')
          mime = if ext ≡ Just "csv"
                 then textCSV
                 else if isApplicationJSON content'
                      then applicationJSON
                      else API.ldJSON
      queryListing $ H.action (Listing.Add fileItem)
      f' ← API.makeFile fileName (CustomData mime content')
      queryListing $ H.action $
        Listing.Filter (not ∘ eq res ∘ itemResource)
      case f' of
        Left err → handleError err
        Right _ →
          void $ queryListing $ H.action $ Listing.Add (Item res)

  where
  isApplicationJSON ∷ String → Boolean
  isApplicationJSON content'
    -- Parse if content is small enough
    | S.length content' < 1048576 = isRight $ jsonParser content'
    -- Or check if its first/last characters are [/]
    | otherwise =
        let trimmed = S.trim content'
        in F.all isJust [S.stripPrefix "[" trimmed, S.stripSuffix "]" trimmed]

  handleError err =
    case GE.fromQError err of
      Left msg → showDialog $ Dialog.Error msg
      Right ge → H.fromAff $ Bus.write ge bus

peek ∷ ∀ a. Bus.BusW GE.GlobalError → ChildQuery a → DSL Unit
peek bus =
  listingPeek bus
  ⨁ searchPeek
  ⨁ const (pure unit)
  ⨁ dialogPeek bus
  ⨁ const (pure unit)

listingPeek ∷ ∀ a. Bus.BusW GE.GlobalError → Listing.QueryP a → DSL Unit
listingPeek bus = go ⨁ (itemPeek bus ∘ H.runChildF)
  where
  go (Listing.Add _ _) = resort
  go (Listing.Adds _ _) = resort
  go _ = pure unit

itemPeek ∷ ∀ a. Bus.BusW GE.GlobalError → Item.Query a → DSL Unit
itemPeek _ (Item.Open res _) = do
  { sort, salt, path } ← H.get
  loc ← H.fromEff locationString
  for_ (preview R._filePath res) \fp →
    showDialog $ Dialog.Explore fp
  for_ (preview R._dirPath res) \dp →
    H.fromEff $ setLocation $ browseURL Nothing sort salt dp
  for_ (preview R._Workspace res) \wp →
    H.fromEff $ setLocation $ append (loc ⊕ "/") $ mkWorkspaceURL wp (Load Editable)


itemPeek bus (Item.Configure (R.Mount mount) _) = configure bus mount
itemPeek _ (Item.Move res _) = do
  showDialog $ Dialog.Rename res
  flip getDirectories rootDir \x →
    void $ queryDialog Dialog.cpRename $ H.action (Rename.AddDirs x)
itemPeek bus (Item.Remove res _) = do
  -- Replace actual item with phantom
  queryListing $ H.action $ Listing.Filter (not ∘ eq res ∘ itemResource)
  queryListing $ H.action $ Listing.Add (PhantomItem res)
  -- Save order of items during deletion (or phantom will be on top of list)
  resort
  -- Try to delete
  mbTrashFolder ← API.delete res
  -- Remove phantom resource after we have response from server
  queryListing $ H.action $ Listing.Filter (not ∘ eq res ∘ itemResource)

  case mbTrashFolder of
    Left err → do
      -- Error occured: put item back and show dialog
      void $ queryListing $ H.action $ Listing.Add (Item res)
      case GE.fromQError err of
        Left msg →
          showDialog $ Dialog.Error msg
        Right ge →
          H.fromAff $ Bus.write ge bus
    Right mbRes →
      -- Item has been deleted: probably add trash folder
      for_ mbRes \res' →
        void $ queryListing $ H.action $ Listing.Add (Item res')

  resort

itemPeek bus (Item.Share res _) = do
  path ← H.gets _.path
  loc ← map (_ ⊕ "/") $ H.fromEff locationString
  for_ (preview R._filePath res) \fp → do
    let newWorkspaceName = Config.newWorkspaceName ⊕ "." ⊕ Config.workspaceExtension
    name ← API.getNewName path newWorkspaceName
    case name of
      Left err →
        case GE.fromQError err of
          Left msg →
            showDialog $ Dialog.Error
              $ "There was a problem creating the workspace: " ⊕ msg
          Right ge →
            H.fromAff $ Bus.write ge bus
      Right name' → do
        showDialog (Dialog.Share $ append loc $  mkWorkspaceURL (path </> dir name') $ Exploring fp)
  for_ (preview R._Workspace res) \wp → do
    showDialog (Dialog.Share $ append loc $ mkWorkspaceURL wp (Load ReadOnly))

itemPeek _ (Item.Download res _) = download res
itemPeek _ _ = pure unit

searchPeek ∷ ∀ a. Search.Query a → DSL Unit
searchPeek (Search.Clear _) = do
  salt ← H.fromEff newSalt
  { sort, path } ← H.get
  H.fromEff $ setLocation $ browseURL Nothing sort salt path
searchPeek (Search.Submit _) = do
  salt ← H.fromEff newSalt
  { sort, path } ← H.get
  value ← H.query' cpSearch unit $ H.request Search.GetValue
  H.fromEff $ setLocation $ browseURL value sort salt path
searchPeek _ = pure unit

dialogPeek ∷ ∀ a. Bus.BusW GE.GlobalError → Dialog.QueryP a → DSL Unit
dialogPeek bus = const (pure unit) ⨁ dialogChildrenPeek bus ∘ H.runChildF

dialogChildrenPeek ∷ ∀ a. Bus.BusW GE.GlobalError → Dialog.ChildQuery a → DSL Unit
dialogChildrenPeek bus q = do
  for_ (prjQuery Dialog.cpMount q) mountPeek
  for_ (prjQuery Dialog.cpExplore q) (explorePeek bus)

explorePeek ∷ ∀ a. Bus.BusW GE.GlobalError → Explore.Query a → DSL Unit
explorePeek bus (Explore.Explore fp name next) = do
  { path } ← H.get
  let newWorkspaceName = name ⊕ "." ⊕ Config.workspaceExtension
  name ← API.getNewName path newWorkspaceName
  case name of
    Left err →
      case GE.fromQError err of
        Left msg →
          showDialog $ Dialog.Error
            $ "There was a problem creating the workspace: " ⊕ msg
        Right ge →
          H.fromAff $ Bus.write ge bus
    Right name' →
      H.fromEff $ setLocation  $ mkWorkspaceURL (path </> dir name') $ Exploring fp
explorePeek _ _ = pure unit

mountPeek ∷ ∀ a. Mount.QueryP a → DSL Unit
mountPeek = go ⨁ const (pure unit)
  where
  go ∷ Mount.Query a → DSL Unit
  go (Mount.NotifySave _) = do
    mount ← queryDialog Dialog.cpMount $ left (H.request Mount.Save)
    for_ (join mount) \m → do
      hideDialog
      -- check if we just edited the mount for the current directory, as if
      -- so, we don't want to add an item to the list for it
      isCurrentMount ← case m of
        R.Database path' → (\p → path' ≡ (p </> dir "")) <$> H.gets _.path
        _ → pure false
      unless isCurrentMount do
        queryListing $ H.action $ Listing.Add $ Item (R.Mount m)
        resort
  go _ = pure unit

dismissSignInSubmenu ∷ DSL Unit
dismissSignInSubmenu = querySignIn $ H.action SignIn.DismissSubmenu
  where
  querySignIn ∷ ∀ a. SignIn.Query a → DSL Unit
  querySignIn =
    void
      ∘ H.query' cpHeader unit
      ∘ right
      ∘ H.ChildF (injSlot Header.cpSignIn unit)
      ∘ right
      ∘ left

updateBreadcrumbs ∷ DSL Unit
updateBreadcrumbs = do
  { path, sort, salt } ← H.get
  void $ H.query' cpBreadcrumbs unit $ H.action (Breadcrumbs.Update path sort salt)

resort ∷ DSL Unit
resort = do
  sort ← H.gets _.sort
  H.query' cpSearch unit (H.request Search.IsSearching)
    >>= traverse_ \isSearching →
      void $ queryListing $ H.action $ Listing.SortBy (sortItem isSearching sort)

configure
  ∷ ∀ r
  . Bus.Bus (write ∷ Bus.Cap | r) GE.GlobalError
  → R.Mount
  → DSL Unit
configure bus (R.View path) = do
  API.viewInfo path >>=
    case _ of
      Left err →
        case GE.fromQError err of
          Left msg →
            showDialog $ Dialog.Error
              $ "There was a problem reading the mount settings: "
              ⊕ msg
          Right ge →
            H.fromAff $ Bus.write ge bus
      Right info →
        showDialog $ Dialog.Mount
          (fromMaybe rootDir (parentDir path))
          (getNameStr (Right path))
          (Just (Right (SQL2.stateFromViewInfo info)))

configure bus (R.Database path) = do
  API.mountInfo path >>=
    case _ of
      Left err →
        case GE.fromQError err of
          Left msg →
            showDialog $ Dialog.Error
              $ "There was a problem reading the mount settings: "
              ⊕ msg
          Right ge →
            H.fromAff $ Bus.write ge bus
      Right config →
        showDialog $ Dialog.Mount
          (fromMaybe rootDir (parentDir path))
          (getNameStr (Left path))
          (Just (Left (MongoDB.fromConfig config)))

download ∷ R.Resource → DSL Unit
download res = do
  hs ← H.fromEff API.authHeaders
  showDialog (Dialog.Download res)
  queryDialog Dialog.cpDownload (H.action $ Download.SetAuthHeaders hs)
  pure unit

getChildren
  ∷ (R.Resource → Boolean)
  → (Array R.Resource → DSL Unit)
  → DirPath
  → DSL Unit
getChildren pred cont start = do
  ei ← API.children start
  case ei of
    Right items → do
      let items' = filter pred items
          parents = mapMaybe (either Just (const Nothing) ∘ R.getPath) items
      cont items'
      traverse_ (getChildren pred cont) parents
    _ → pure unit

getDirectories ∷ (Array R.Resource → DSL Unit) → DirPath → DSL Unit
getDirectories = getChildren (R.isDirectory ∨ R.isDatabaseMount)

showDialog ∷ Dialog.Dialog → DSL Unit
showDialog = void ∘ H.query' cpDialog unit ∘ left ∘ H.action ∘ Dialog.Show

hideDialog ∷ DSL Unit
hideDialog = void $ H.query' cpDialog unit $ left (H.action Dialog.Dismiss)

queryListing ∷ ∀ a. Listing.Query a → DSL (Maybe a)
queryListing = H.query' cpListing unit ∘ left

queryItem ∷ ∀ a. Listing.ItemSlot → Item.Query a → DSL (Maybe a)
queryItem slot = H.query' cpListing unit ∘ right ∘ H.ChildF slot

queryDialog
  ∷ ∀ s f a
  . ChildPath s Dialog.ChildState f Dialog.ChildQuery Unit Dialog.ChildSlot
  → f a
  → DSL (Maybe a)
queryDialog cp =
  H.query' cpDialog unit ∘ right ∘ H.ChildF (injSlot cp unit) ∘ injQuery cp
