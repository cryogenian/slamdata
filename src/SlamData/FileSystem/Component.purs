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

import Prelude

import Control.Apply ((*>))
import Control.Bind (join, (=<<))
import Control.Monad (when)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Error.Class (throwError)
import Control.UI.Browser (setLocation, locationString, clearValue)
import Control.UI.Browser.Event as Be
import Control.UI.File as Cf

import Data.Array (head, last, mapMaybe, filter)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, isRight)
import Data.Foldable (traverse_, for_)
import Data.Functor (($>))
import Data.Functor.Aff (liftAff)
import Data.Functor.Coproduct (left, right, coproduct)
import Data.Functor.Coproduct.Nested (coproduct5)
import Data.Functor.Eff (liftEff)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Path.Pathy (rootDir, (</>), dir, file, parentDir)
import Data.String as S
import Data.URI (runParseAbsoluteURI)

import Halogen.Component
import Halogen.Component.ChildPath (ChildPath(), injSlot, prjQuery, injQuery)
import Halogen.Component.Utils (forceRerender')
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Query (action, request, get, modify, gets)
import Halogen.Themes.Bootstrap3 as B

import Network.HTTP.MimeType.Common (textCSV, applicationJSON)

import Quasar.Aff as API
import Quasar.Auth as Auth

import SlamData.Config as Config
import SlamData.Effects (Slam())
import SlamData.FileSystem.Breadcrumbs.Component as Breadcrumbs
import SlamData.FileSystem.Component.Install
import SlamData.FileSystem.Component.Query
import SlamData.FileSystem.Component.Render
import SlamData.FileSystem.Component.State
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
import SlamData.Render.Common
import SlamData.Render.CSS as Rc
import SlamData.SignIn.Component as SignIn

import Utils.DOM as D
import Utils.Path (DirPath(), getNameStr)

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval (peek <<< runChildF)

render :: RenderParent State ChildState Query ChildQuery Slam ChildSlot
render state@{ version, sort, salt, path } =
  H.div
    [ P.classes [ Rc.filesystem ]
    , E.onClick (E.input_ DismissSignInSubmenu)
    ]
    [ navbar
        [ H.div
            [ P.classes [ Rc.header, B.clearfix ] ]
            [ icon B.glyphiconFolderOpen Config.homeHash "Browse root folder"
            , logo version
            , H.slot' cpSearch unit \_ ->
                { component: Search.comp
                , initialState: Search.initialState sort salt
                }
            , H.slot' cpSignIn unit \_ ->
                { component: SignIn.comp
                , initialState: installedState SignIn.initialState
                }
            ]
        ]
    , content
        [ H.div
            [ P.class_ B.clearfix ]
            [ H.slot' cpBreadcrumbs unit \_ ->
                { component: Breadcrumbs.comp
                , initialState: Breadcrumbs.mkBreadcrumbs path sort salt
                }
            , toolbar state
            ]
        , row [ sorting state ]
        , H.slot' cpListing unit \_ ->
            { component: Listing.comp
            , initialState: installedState Listing.initialState
            }
        ]
    , H.slot' cpDialog unit \_ ->
        { component: Dialog.comp
        , initialState: installedState Dialog.initialState
        }
    ]

eval :: EvalParent Query State ChildState Query ChildQuery Slam ChildSlot
eval (Resort next) = do
  { sort, salt, path } <- get
  searchValue <- query' cpSearch unit (request Search.GetValue)
  liftEff $ setLocation $ browseURL (join searchValue) (notSort sort) salt path
  pure next
eval (SetPath path next) = modify (_path .~ path) *> updateBreadcrumbs $> next
eval (SetSort sort next) = do
  modify (_sort .~ sort)
  updateBreadcrumbs
  resort
  pure next
eval (SetSalt salt next) = modify (_salt .~ salt) *> updateBreadcrumbs $> next
eval (SetIsMount isMount next) = modify (_isMount .~ isMount) $> next
eval (ShowHiddenFiles next) = do
  modify (_showHiddenFiles .~ true)
  queryListing $ action (Listing.SetIsHidden false)
  pure next
eval (HideHiddenFiles next) = do
  modify (_showHiddenFiles .~ false)
  queryListing $ action (Listing.SetIsHidden true)
  pure next
eval (Configure next) = do
  path <- gets _.path
  configure (R.Database path)
  pure next
eval (MakeMount next) = do
  path <- gets _.path
  showDialog (Dialog.Mount path "" Nothing)
  pure next
eval (MakeFolder next) = do
  path <- gets _.path
  dirName <- liftAff $ Auth.authed $ API.getNewName path Config.newFolderName
  let dirPath = path </> dir dirName
      dirRes = R.Directory dirPath
      dirItem = PhantomItem dirRes
      hiddenFile = dirPath </> file (Config.folderMark)
  queryListing $ action (Listing.Add dirItem)
  added <- liftAff $ attempt $
    Auth.authed $ API.makeFile hiddenFile API.ldJSON "{}"
  queryListing $ action (Listing.Filter (/= dirItem))
  case added of
    Left err ->
      showDialog $
        Dialog.Error
          $ "There was a problem creating the directory: "
          <> message err
    Right _ -> do
      void $ queryListing $ action $ Listing.Add (Item dirRes)
  pure next

eval (MakeNotebook next) = do
  path <- gets _.path
  let newNotebookName = Config.newNotebookName <> "." <> Config.notebookExtension
  name <- liftAff $ Auth.authed $ API.getNewName path newNotebookName
  let uri = mkNotebookURL (path </> dir name) New
  liftEff $ setLocation uri
  pure next

eval (UploadFile el next) = do
  mbInput <- liftEff $ D.querySelector "input" el
  for_ mbInput \input ->
    void $ liftEff $ Be.raiseEvent "click" input
  pure next

eval (FileListChanged el next) = do
  fileArr <- map Cf.fileListToArray $ (liftAff $ Cf.files el)
  liftEff $ clearValue el
  case head fileArr of
    Nothing ->
      let err :: Slam Unit
          err = throwError $ error "empty filelist"
      in liftAff err
    Just f -> do
      { path, sort, salt } <- get
      name <- liftAff $ liftEff (Cf.name f)
                >>= Auth.authed <<< API.getNewName path

      reader <- liftEff Cf.newReaderEff
      content <- liftAff $ Cf.readAsBinaryString f reader

      let fileName = path </> file name
          res = R.File fileName
          fileItem = PhantomItem res
          ext = last (S.split "." name)
          mime = if ext == Just "csv"
                 then textCSV
                 else if isApplicationJSON content
                      then applicationJSON
                      else API.ldJSON
      queryListing $ action (Listing.Add fileItem)
      f <- liftAff $ attempt $ Auth.authed $ API.makeFile fileName mime content
      case f of
        Left err -> do
          queryListing $ action $
            Listing.Filter (not <<< eq (R.File fileName) <<< itemResource)
          showDialog $ Dialog.Error (message err)
        Right _ -> liftEff $ openItem res sort salt

  pure next
  where
  isApplicationJSON :: String -> Boolean
  isApplicationJSON content
    -- Parse if content is small enough
    | S.length content < 1048576 =
        isRight $ jsonParser content
    -- Or check if its first/last characters are [/]
    | otherwise =
        let
          trimedContent = S.trim content
        in
             (isJust $ S.stripPrefix "[" trimedContent)
          && (isJust $ S.stripSuffix "]" trimedContent)



eval (Download next) = do
  path <- gets _.path
  download (R.Directory path)
  pure next

eval (SetVersion version next) = modify (_version .~ Just version) $> next
eval (DismissSignInSubmenu next) = dismissSignInSubmenu $> next

peek :: forall a. ChildQuery a -> Algebra Unit
peek =
  coproduct5
    listingPeek
    searchPeek
    (const (pure unit))
    dialogPeek
    (const (pure unit))

listingPeek :: forall a. Listing.QueryP a -> Algebra Unit
listingPeek = coproduct go (itemPeek <<< runChildF)
  where
  go (Listing.Add _ _) = resort
  go (Listing.Adds _ _) = resort
  go _ = pure unit

itemPeek :: forall a. Item.Query a -> Algebra Unit
itemPeek (Item.Open res _) = do
  { sort, salt } <- get
  liftEff $ openItem res sort salt
itemPeek (Item.Configure (R.Mount mount) _) = configure mount
itemPeek (Item.Move res _) = do
  showDialog $ Dialog.Rename res
  flip getDirectories rootDir \x ->
    void $ queryDialog Dialog.cpRename $ action (Rename.AddDirs x)
itemPeek (Item.Remove res _) = do
  mbTrashFolder <- liftAff $ Auth.authed $ API.delete res
  queryListing $ action $ Listing.Filter (not <<< eq res <<< itemResource)
  case mbTrashFolder of
    Nothing -> pure unit
    Just res' -> void $ queryListing $ action $ Listing.Add (Item res')
itemPeek (Item.Share res _) = do
  { sort, salt } <- get
  loc <- liftEff locationString
  let url = loc <> "/" <> itemURL sort salt ReadOnly res
  showDialog (Dialog.Share url)
itemPeek (Item.Download res _) = download res
itemPeek _ = pure unit

searchPeek :: forall a. Search.Query a -> Algebra Unit
searchPeek (Search.Clear _) = do
  salt <- liftEff newSalt
  { sort, path } <- get
  liftEff $ setLocation $ browseURL Nothing sort salt path
searchPeek _ = pure unit

dialogPeek :: forall a. Dialog.QueryP a -> Algebra Unit
dialogPeek = coproduct (const (pure unit)) (dialogChildrenPeek <<< runChildF)

dialogChildrenPeek :: forall a. Dialog.ChildQuery a -> Algebra Unit
dialogChildrenPeek q =
  fromMaybe (pure unit) (mountPeek <$> prjQuery Dialog.cpMount q)

mountPeek :: forall a. Mount.QueryP a -> Algebra Unit
mountPeek = coproduct go (const (pure unit))
  where
  go :: Mount.Query a -> Algebra Unit
  go (Mount.NotifySave _) = do
    mount <- queryDialog Dialog.cpMount $ left (request Mount.Save)
    case join mount of
      Nothing -> pure unit
      Just m -> do
        hideDialog
        -- check if we just edited the mount for the current directory, as if
        -- so, we don't want to add an item to the list for it
        isCurrentMount <- case m of
          R.Database path' -> (\p -> path' == (p </> dir "")) <$> gets _.path
          _ -> pure false
        when (not isCurrentMount) do
          queryListing $ action $ Listing.Add $ Item (R.Mount m)
          resort
  go _ = pure unit

dismissSignInSubmenu :: Algebra Unit
dismissSignInSubmenu = querySignIn $ action SignIn.DismissSubmenu
  where
  querySignIn :: forall a. SignIn.Query a -> Algebra Unit
  querySignIn q = query' cpSignIn unit (left q) $> unit

updateBreadcrumbs :: Algebra Unit
updateBreadcrumbs = do
  { path, sort, salt } <- get
  void $ query' cpBreadcrumbs unit $ action (Breadcrumbs.Update path sort salt)

resort :: Algebra Unit
resort = do
  sort <- gets _.sort
  mbIsSearching <- query' cpSearch unit (request Search.IsSearching)
  case mbIsSearching of
    Nothing -> pure unit
    Just isSearching ->
      void $ queryListing $ action $ Listing.SortBy (sortItem isSearching sort)

configure :: R.Mount -> Algebra Unit
configure (R.View path) = do
  viewInfo <- liftAff $ attempt $ Auth.authed $ API.viewInfo path
  showDialog
    case viewInfo of
      Left err ->
        Dialog.Error
          $ "There was a problem reading the mount settings: "
          <> show err
      Right info ->
        Dialog.Mount
          (fromMaybe rootDir (parentDir path))
          (getNameStr (Left path))
          (Just (Right (SQL2.stateFromViewInfo info)))

configure (R.Database path) = do
  viewInfo <- liftAff $ attempt $ Auth.authed $ API.mountInfo path
  showDialog
    case map (lmap show) runParseAbsoluteURI =<< lmap show viewInfo of
      Left err ->
        Dialog.Error
          $ "There was a problem reading the mount settings: " <> err
      Right uri ->
        Dialog.Mount
          (fromMaybe rootDir (parentDir path))
          (getNameStr (Right path))
          (Just (Left (MongoDB.stateFromURI uri)))

download :: R.Resource -> Algebra Unit
download res = do
  showDialog (Dialog.Download res)
  forceRerender'
  hs <- liftEff Auth.authHeaders
  queryDialog Dialog.cpDownload (action $ Download.SetAuthHeaders hs)
  getChildren
    (const true)
    (void <<< queryDialog Dialog.cpDownload <<< action <<< Download.AddSources)
    rootDir
  pure unit

getChildren
  :: (R.Resource -> Boolean)
  -> (Array R.Resource -> Algebra Unit)
  -> DirPath
  -> Algebra Unit
getChildren pred cont start = do
  forceRerender'
  ei <- liftAff $ attempt $ Auth.authed $ API.children start
  case ei of
    Right items -> do
      let items' = filter pred items
          parents = mapMaybe (either (const Nothing) Just <<< R.getPath) items
      cont items'
      traverse_ (getChildren pred cont) parents
    _ -> pure unit

getDirectories :: (Array R.Resource -> Algebra Unit) -> DirPath -> Algebra Unit
getDirectories = getChildren (R.isDirectory || R.isDatabaseMount)

showDialog :: Dialog.Dialog -> Algebra Unit
showDialog = void <<< query' cpDialog unit <<< left <<< action <<< Dialog.Show

hideDialog :: Algebra Unit
hideDialog = void $ query' cpDialog unit $ left (action Dialog.Dismiss)

queryListing :: forall a. Listing.Query a -> Algebra (Maybe a)
queryListing = query' cpListing unit <<< left

queryItem :: forall a. Listing.ItemSlot -> Item.Query a -> Algebra (Maybe a)
queryItem slot = query' cpListing unit <<< right <<< ChildF slot

queryDialog
  :: forall s f a
   . ChildPath s Dialog.ChildState f Dialog.ChildQuery Unit Dialog.ChildSlot
  -> f a
  -> Algebra (Maybe a)
queryDialog cp =
  query' cpDialog unit <<< right <<< ChildF (injSlot cp unit) <<< injQuery cp
