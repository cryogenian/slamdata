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

import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Error.Class (throwError)
import Control.UI.Browser (setLocation, locationString, clearValue)
import Control.UI.Browser.Event as Be
import Control.UI.File as Cf

import Data.Argonaut (jsonParser, jsonEmptyObject)
import Data.Array (head, last, mapMaybe, filter)
import Data.Foldable as F
import Data.Functor.Coproduct.Nested (coproduct5)
import Data.Lens ((.~))
import Data.MediaType.Common (textCSV, applicationJSON)
import Data.Path.Pathy (rootDir, (</>), dir, file, parentDir)
import Data.String as S
import Data.String.Regex as Rgx

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, injSlot, prjQuery, injQuery)
import Halogen.Component.Utils (forceRerender')
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import Quasar.Data (QData(..))

import SlamData.Config as Config
import SlamData.Effects (Slam)
import SlamData.FileSystem.Breadcrumbs.Component as Breadcrumbs
import SlamData.FileSystem.Component.Install (Algebra, ChildQuery, ChildSlot, ChildState, QueryP, StateP, cpBreadcrumbs, cpDialog, cpListing, cpSearch, cpSignIn, toDialog, toFs, toListing, toSearch, toSignIn)
import SlamData.FileSystem.Component.Query (Query(..))
import SlamData.FileSystem.Component.Render (sorting, toolbar)
import SlamData.FileSystem.Component.State (State, _isMount, _path, _salt, _showHiddenFiles, _sort, _version, initialState)
import SlamData.FileSystem.Dialog.Component as Dialog
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State as MongoDB
import SlamData.FileSystem.Dialog.Mount.SQL2.Component.State as SQL2
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Listing.Component as Listing
import SlamData.FileSystem.Listing.Item (Item(..), itemResource, itemURL, openItem, sortItem)
import SlamData.FileSystem.Listing.Item.Component as Item
import SlamData.FileSystem.Listing.Sort (notSort)
import SlamData.FileSystem.Resource as R
import SlamData.FileSystem.Routing (browseURL)
import SlamData.FileSystem.Routing.Salt (newSalt)
import SlamData.FileSystem.Search.Component as Search
import SlamData.Notebook.Action (Action(..), AccessType(..))
import SlamData.Notebook.Routing (mkNotebookURL)
import SlamData.Quasar (ldJSON) as API
import SlamData.Quasar.Data (makeFile, save) as API
import SlamData.Quasar.FS (children, delete, getNewName) as API
import SlamData.Quasar.Mount (mountInfo, viewInfo) as API
import SlamData.Render.Common (navbar, logo, icon, content, row)
import SlamData.Render.CSS as Rc
import SlamData.SignIn.Component as SignIn

import Utils.DOM as D
import Utils.Path (DirPath, getNameStr)

type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }

render ∷ State → HTML
render state@{ version, sort, salt, path } =
  HH.div
    [ HP.classes [ Rc.filesystem ]
    , HE.onClick (HE.input_ DismissSignInSubmenu)
    ]
    [ navbar
        [ HH.div
            [ HP.classes [ Rc.header, B.clearfix ] ]
            [ icon B.glyphiconFolderOpen Config.homeHash "Browse root folder"
            , logo version
            , HH.slot' cpSearch unit \_ →
                { component: Search.comp
                , initialState: Search.initialState sort salt
                }
            , HH.slot' cpSignIn unit \_ →
                { component: SignIn.comp
                , initialState: H.parentState SignIn.initialState
                }
            ]
        ]
    , content
        [ HH.div
            [ HP.class_ B.clearfix ]
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
        { component: Dialog.comp
        , initialState: H.parentState Dialog.initialState
        }
    ]

eval ∷ Natural Query DSL
eval (Resort next) = do
  { sort, salt, path } ← H.get
  searchValue ← H.query' cpSearch unit (H.request Search.GetValue)
  H.fromEff $ setLocation $ browseURL (join searchValue) (notSort sort) salt path
  pure next
eval (SetPath path next) = H.modify (_path .~ path) *> updateBreadcrumbs $> next
eval (SetSort sort next) = do
  H.modify (_sort .~ sort)
  updateBreadcrumbs
  resort
  pure next
eval (SetSalt salt next) = H.modify (_salt .~ salt) *> updateBreadcrumbs $> next
eval (SetIsMount isMount next) = H.modify (_isMount .~ isMount) $> next
eval (ShowHiddenFiles next) = do
  H.modify (_showHiddenFiles .~ true)
  queryListing $ H.action (Listing.SetIsHidden false)
  pure next
eval (HideHiddenFiles next) = do
  H.modify (_showHiddenFiles .~ false)
  queryListing $ H.action (Listing.SetIsHidden true)
  pure next
eval (Configure next) = do
  path ← H.gets _.path
  configure (R.Database path)
  pure next
eval (MakeMount next) = do
  path ← H.gets _.path
  showDialog (Dialog.Mount path "" Nothing)
  pure next
eval (MakeFolder next) = do
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
      showDialog $ Dialog.Error
        $ "There was a problem creating the directory: "
        ⊕ message err
    Right dirRes →
      void $ queryListing $ H.action $ Listing.Add (Item dirRes)
  pure next

eval (MakeNotebook next) = do
  path ← H.gets _.path
  let newNotebookName = Config.newNotebookName ⊕ "." ⊕ Config.notebookExtension
  name ← API.getNewName path newNotebookName
  case name of
    Left err →
      -- This error isn't strictly true as we're not actually creating the
      -- notebook here, but saying there was a problem "creating a name for the
      -- notebook" would be a little strange
      showDialog $ Dialog.Error
        $ "There was a problem creating the notebook: "
        ⊕ message err
    Right name' → do
      H.fromEff $ setLocation $ mkNotebookURL (path </> dir name') New
  pure next

eval (UploadFile el next) = do
  mbInput ← H.fromEff $ D.querySelector "input" el
  for_ mbInput \input →
    void $ H.fromEff $ Be.raiseEvent "click" input
  pure next

eval (FileListChanged el next) = do
  fileArr ← map Cf.fileListToArray $ (H.fromAff $ Cf.files el)
  H.fromEff $ clearValue el
  case head fileArr of
    Nothing →
      let err ∷ Slam Unit
          err = throwError $ error "empty filelist"
      in H.fromAff err
    Just f → uploadFileSelected f
  pure next

eval (Download next) = do
  path ← H.gets _.path
  download (R.Directory path)
  pure next

eval (SetVersion version next) = H.modify (_version .~ Just version) $> next
eval (DismissSignInSubmenu next) = dismissSignInSubmenu $> next

uploadFileSelected ∷ Cf.File → DSL Unit
uploadFileSelected f = do
  { path, sort, salt } ← H.get
  name ←
    H.fromEff (Cf.name f)
      <#> Rgx.replace (Rgx.regex "/" Rgx.noFlags{global=true}) ":"
      >>= API.getNewName path

  case name of
    Left err → showDialog $ Dialog.Error (message err)
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
      case f' of
        Left err → do
          queryListing $ H.action $
            Listing.Filter (not ∘ eq (R.File fileName) ∘ itemResource)
          showDialog $ Dialog.Error (message err)
        Right _ → H.fromEff $ openItem res sort salt
  where
  isApplicationJSON ∷ String → Boolean
  isApplicationJSON content'
    -- Parse if content is small enough
    | S.length content' < 1048576 = isRight $ jsonParser content'
    -- Or check if its first/last characters are [/]
    | otherwise =
        let trimmed = S.trim content'
        in F.all isJust [S.stripPrefix "[" trimmed, S.stripSuffix "]" trimmed]

peek ∷ ∀ a. ChildQuery a → DSL Unit
peek =
  coproduct5
    listingPeek
    searchPeek
    (const (pure unit))
    dialogPeek
    (const (pure unit))

listingPeek ∷ ∀ a. Listing.QueryP a → DSL Unit
listingPeek = coproduct go (itemPeek ∘ H.runChildF)
  where
  go (Listing.Add _ _) = resort
  go (Listing.Adds _ _) = resort
  go _ = pure unit

itemPeek ∷ ∀ a. Item.Query a → DSL Unit
itemPeek (Item.Open res _) = do
  { sort, salt } ← H.get
  H.fromEff $ openItem res sort salt
itemPeek (Item.Configure (R.Mount mount) _) = configure mount
itemPeek (Item.Move res _) = do
  showDialog $ Dialog.Rename res
  flip getDirectories rootDir \x →
    void $ queryDialog Dialog.cpRename $ H.action (Rename.AddDirs x)
itemPeek (Item.Remove res _) = do
  mbTrashFolder ← API.delete res
  queryListing $ H.action $ Listing.Filter (not ∘ eq res ∘ itemResource)
  case mbTrashFolder of
    Left err → showDialog $ Dialog.Error (message err)
    Right (Nothing) → pure unit
    Right (Just res') → void $ queryListing $ H.action $ Listing.Add (Item res')
itemPeek (Item.Share res _) = do
  { sort, salt } ← H.get
  loc ← H.fromEff locationString
  let url = loc ⊕ "/" ⊕ itemURL sort salt ReadOnly res
  showDialog (Dialog.Share url)
itemPeek (Item.Download res _) = download res
itemPeek (Item.SharePermissions res _) = do
  showDialog $ Dialog.Permissions res
itemPeek _ = pure unit

searchPeek ∷ ∀ a. Search.Query a → DSL Unit
searchPeek (Search.Clear _) = do
  salt ← H.fromEff newSalt
  { sort, path } ← H.get
  H.fromEff $ setLocation $ browseURL Nothing sort salt path
searchPeek _ = pure unit

dialogPeek ∷ ∀ a. Dialog.QueryP a → DSL Unit
dialogPeek = coproduct (const (pure unit)) (dialogChildrenPeek ∘ H.runChildF)

dialogChildrenPeek ∷ ∀ a. Dialog.ChildQuery a → DSL Unit
dialogChildrenPeek q =
  fromMaybe (pure unit) (mountPeek <$> prjQuery Dialog.cpMount q)

mountPeek ∷ ∀ a. Mount.QueryP a → DSL Unit
mountPeek = coproduct go (const (pure unit))
  where
  go ∷ Mount.Query a → DSL Unit
  go (Mount.NotifySave _) = do
    mount ← queryDialog Dialog.cpMount $ left (H.request Mount.Save)
    case join mount of
      Nothing → pure unit
      Just m → do
        hideDialog
        -- check if we just edited the mount for the current directory, as if
        -- so, we don't want to add an item to the list for it
        isCurrentMount ← case m of
          R.Database path' → (\p → path' ≡ (p </> dir "")) <$> H.gets _.path
          _ → pure false
        when (not isCurrentMount) do
          queryListing $ H.action $ Listing.Add $ Item (R.Mount m)
          resort
  go _ = pure unit

dismissSignInSubmenu ∷ DSL Unit
dismissSignInSubmenu = querySignIn $ H.action SignIn.DismissSubmenu
  where
  querySignIn ∷ ∀ a. SignIn.Query a → DSL Unit
  querySignIn q = H.query' cpSignIn unit (left q) $> unit

updateBreadcrumbs ∷ DSL Unit
updateBreadcrumbs = do
  { path, sort, salt } ← H.get
  void $ H.query' cpBreadcrumbs unit $ H.action (Breadcrumbs.Update path sort salt)

resort ∷ DSL Unit
resort = do
  sort ← H.gets _.sort
  mbIsSearching ← H.query' cpSearch unit (H.request Search.IsSearching)
  case mbIsSearching of
    Nothing → pure unit
    Just isSearching →
      void $ queryListing $ H.action $ Listing.SortBy (sortItem isSearching sort)

configure ∷ R.Mount → DSL Unit
configure (R.View path) = do
  viewInfo ← API.viewInfo path
  showDialog
    case viewInfo of
      Left err →
        Dialog.Error
          $ "There was a problem reading the mount settings: "
          ⊕ show err
      Right info →
        Dialog.Mount
          (fromMaybe rootDir (parentDir path))
          (getNameStr (Right path))
          (Just (Right (SQL2.stateFromViewInfo info)))

configure (R.Database path) = do
  viewInfo ← API.mountInfo path
  showDialog
    case viewInfo of
      Left err →
        Dialog.Error
          $ "There was a problem reading the mount settings: "
          ⊕ message err
      Right config →
        Dialog.Mount
          (fromMaybe rootDir (parentDir path))
          (getNameStr (Left path))
          (Just (Left (MongoDB.fromConfig config)))

download ∷ R.Resource → DSL Unit
download res = do
  showDialog (Dialog.Download res)
  forceRerender'
  getChildren
    (const true)
    (void ∘ queryDialog Dialog.cpDownload ∘ H.action ∘ Download.AddSources)
    rootDir
  pure unit

getChildren
  ∷ (R.Resource → Boolean)
  → (Array R.Resource → DSL Unit)
  → DirPath
  → DSL Unit
getChildren pred cont start = do
  forceRerender'
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
