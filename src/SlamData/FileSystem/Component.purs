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

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Bind (join)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Error.Class (throwError)
import Control.UI.Browser (setLocation, locationString, clearValue)
import Control.UI.Browser.Event as Be
import Control.UI.File as Cf

import Data.Array (head, last, mapMaybe, filter)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Functor.Aff (liftAff)
import Data.Functor.Coproduct (left, right, coproduct)
import Data.Functor.Eff (liftEff)
import Data.Inject (prj)
import Data.Lens ((^.), (.~), preview)
import Data.Lens.Prism.Coproduct (_Left, _Right)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Path.Pathy (rootDir, (</>), dir, file)
import Data.String as S
import Data.URI (runParseAbsoluteURI)

import Halogen.Component
import Halogen.Component.ChildPath (injSlot, prjSlot, prjQuery, injQuery)
import Halogen.Component.Utils (applyCF, forceRerender')
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Query (action, request, get, modify, gets)
import Halogen.Themes.Bootstrap3 as B

import Network.HTTP.MimeType.Common (textCSV)

import Quasar.Aff as API
import Quasar.Auth as Auth

import SlamData.Config as Config
import SlamData.FileSystem.Breadcrumbs.Component as Breadcrumbs
import SlamData.FileSystem.Component.Install
import SlamData.FileSystem.Component.Query
import SlamData.FileSystem.Component.Render
import SlamData.FileSystem.Component.State
import SlamData.FileSystem.Dialog.Component as Dialog
import SlamData.FileSystem.Dialog.Download.Component as Download
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.SQLMount.Component as SQLMount
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.Effects (Slam())
import SlamData.SignIn.Component as SignIn
import SlamData.FileSystem.Listing.Component as Items
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

import Utils.DOM as D
import Utils.Path (DirPath())

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: RenderParent State ChildState Query ChildQuery Slam ChildSlot
render state@(State r) =
  H.div
    [ P.classes [ Rc.filesystem ]
    , E.onClick (E.input_ DismissSignInSubmenu)
    ]
    [ navbar
      [  H.div [ P.classes [ Rc.header, B.clearfix ] ]
         [ icon B.glyphiconFolderOpen Config.homeHash
         , logo (state ^. _version)
         , H.slot' cpSearch SearchSlot
           \_ -> { component: Search.comp
                 , initialState: Search.initialState r.sort  r.salt
                 }
         , H.slot' cpSignIn unit \_ ->
             { component: SignIn.comp
             , initialState: installedState SignIn.initialState
             }
         ]
      ]
    , content
      [ H.div [ P.class_ B.clearfix ]
        [ H.slot' cpBreadcrumbs (BreadcrumbsSlot r.path)
          \_ -> { component: Breadcrumbs.comp
                , initialState: Breadcrumbs.mkBreadcrumbs r.path r.sort r.salt
                }
        , toolbar state
        ]
      , row [ sorting state ]
      , H.slot' cpItems ItemsSlot
        \_ -> { component: Items.comp
              , initialState: installedState $ Items.initialState
              }
      ]
    , H.slot' cpDialog DialogSlot
      \_ -> { component: Dialog.comp
            , initialState: installedState $ Dialog.initialState
            }
    ]

eval :: EvalParent Query State ChildState Query ChildQuery Slam ChildSlot
eval (Resort next) = do
  searchValue <- query' cpSearch SearchSlot (request Search.GetValue)
  state <- get
  liftEff $ setLocation $ browseURL (searchValue >>= id)
    (notSort (state ^. _sort)) (state ^. _salt) (state ^. _path)
  pure next
eval (SetPath path next) = do
  modify (_path .~ path)
  pure next
eval (SetSort sort next) = do
  modify (_sort .~ sort)
  resort
  pure next
eval (SetSalt salt next) = do
  modify (_salt .~ salt)
  pure next
eval (SetIsMount isMount next) = do
  modify (_isMount .~ isMount)
  pure next
eval (ShowHiddenFiles next) = do
  modify (_showHiddenFiles .~ true)
  query' cpItems ItemsSlot $ left $ action $ Items.SetIsHidden false
  pure next
eval (HideHiddenFiles next) = do
  modify (_showHiddenFiles .~ false)
  query' cpItems ItemsSlot $ left $ action $ Items.SetIsHidden true
  pure next
eval (Configure next) = do
  state <- get
  let res = R.Database $ state ^. _path
  eiURI <- liftAff $ attempt $ Auth.authed $ API.mountInfo res
  configure (either (const Nothing) Just eiURI) res
  pure next
eval (MakeMount next) = do
  state <- get
  query' cpDialog DialogSlot $ left $ action
    $ Dialog.Show (Dialog.Mount (state ^. _path))
  pure next
eval (MakeSQLView next) = do
  state <- get
  query' cpDialog DialogSlot $ left $ action
    $ Dialog.Show (Dialog.SQLMount (state ^. _path))
  pure next
eval (MakeFolder next) = do
  state <- get
  let path = state ^. _path
  dirName <- liftAff $ Auth.authed $ API.getNewName path Config.newFolderName
  let dirPath = path </> dir dirName
      dirRes = R.Directory dirPath
      dirItem = PhantomItem dirRes
      hiddenFile = dirPath </> file (Config.folderMark)
  query' cpItems ItemsSlot $ left $ action $ Items.Add dirItem
  added <- liftAff do
    attempt $ Auth.authed $ API.makeFile hiddenFile API.ldJSON "{}"
  query' cpItems ItemsSlot $ left $ action
    $ Items.Filter (/= dirItem)
  case added of
    Left err ->
      query' cpDialog DialogSlot $ left $ action
      $ Dialog.Show (Dialog.Error $ "There was a problem creating the directory: "
              <> message err)
    Right _ -> do
      query' cpItems ItemsSlot $ left $ action $ Items.Add $ Item dirRes
  pure next

eval (MakeNotebook next) = do
  path <- gets (^. _path)
  let newNotebookName = Config.newNotebookName <> "." <> Config.notebookExtension
  name <- liftAff $ Auth.authed $ API.getNewName path newNotebookName
  let uri = mkNotebookURL (path </> dir name) New
  liftEff $ setLocation uri
  pure next

eval (UploadFile el next) = do
  mbInput <- liftEff $ D.querySelector "input" el
  case mbInput of
    Nothing -> pure unit
    Just input -> void $ liftEff $ Be.raiseEvent "click" input
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
      state <- get
      let path = state ^. _path
      name <- liftAff $ liftEff (Cf.name f)
                >>= Auth.authed <<< API.getNewName (state ^. _path)

      let fileName = path </> file name
          fileItem = PhantomItem $ R.File fileName
          ext = last (S.split "." name)
          mime = if ext == Just "csv"
                 then textCSV
                 else API.ldJSON
      reader <- liftEff Cf.newReaderEff
      content <- liftAff $ Cf.readAsBinaryString f reader
      query' cpItems ItemsSlot $ left $ action $ Items.Add fileItem
      f <- liftAff $ attempt $ Auth.authed $ API.makeFile fileName mime content
      case f of
        Left err -> do
          query' cpItems ItemsSlot $ left $ action
            $ Items.Filter (not <<< eq (R.File fileName) <<< itemResource)
          query' cpDialog DialogSlot $ left $ action
            $ Dialog.Show (Dialog.Error $ message err)
          pure unit
        Right _ ->
          liftEff $ setLocation
            $ itemURL (state ^. _sort) (state ^. _salt) Editable fileItem

  pure next

eval (Download next) = do
  state <- get
  download (R.Directory (state ^. _path))
  pure next

eval (SetVersion version next) = do
  modify (_version .~ Just version)
  pure next

eval (DismissSignInSubmenu next) = dismissSignInSubmenu *> pure next

dismissSignInSubmenu :: Algebra Unit
dismissSignInSubmenu = querySignIn $ action SignIn.DismissSubmenu
  where
  querySignIn :: forall a. SignIn.Query a -> Algebra Unit
  querySignIn q = query' cpSignIn unit (left q) *> pure unit

peek :: forall a. ChildF ChildSlot ChildQuery a -> Algebra Unit
peek (ChildF p q) =
  fromMaybe (pure unit)
  $   (itemsPeek <$> prjSlot cpItems p <*> prjQuery cpItems q)
  <|> (searchPeek <$> prjSlot cpSearch p <*> prjQuery cpSearch q)
  <|> (dialogPeek <$> prjSlot cpDialog p <*> prjQuery cpDialog q)

dialogPeek :: forall a. DialogSlot -> Dialog.QueryP a -> Algebra Unit
dialogPeek s q =
  fromMaybe (pure unit)
  $   (applyCF dialogChildrenPeek <$> prj q)

dialogChildrenPeek ::
  forall a. Dialog.ChildSlot -> Dialog.ChildQuery a -> Algebra Unit
dialogChildrenPeek p q =
  fromMaybe (pure unit)
  $   (mountPeek <$> prjSlot Dialog.cpMount p <*> prjQuery Dialog.cpMount q)
  <|> (sqlMountPeek <$> prjSlot Dialog.cpSQLMount p <*> prjQuery Dialog.cpSQLMount q)

sqlMountPeek :: forall a. Dialog.SQLMountSlot -> SQLMount.QueryP a -> Algebra Unit
sqlMountPeek slot q = coproduct sqlMountPeekExact (const $ pure unit) q
  where
  sqlMountPeekExact (SQLMount.Success res _) = do
    void $ query' cpItems ItemsSlot $ left $ action $ Items.Add $ Item res
  sqlMountPeekExact _ = pure unit

mountPeek :: forall a. Dialog.MountSlot -> Mount.Query a -> Algebra Unit
mountPeek slot (Mount.Save _) = do
  saved <- query' cpDialog DialogSlot $ right
           $ ChildF (injSlot Dialog.cpMount slot)
           $ injQuery Dialog.cpMount (request Mount.GetSaved)
  case join saved of
    Nothing -> pure unit
    Just res ->
      void $ query' cpItems ItemsSlot $ left $ action $ Items.Add $ Item res
mountPeek _ _ = pure unit

itemsPeek :: forall a. ItemsSlot -> Items.QueryP a -> Algebra Unit
itemsPeek s q =
  fromMaybe (pure unit)
  $   (itemsPeek' s <$> preview _Left q)
  <|> (applyCF itemPeek <$> preview _Right q)

itemsPeek' :: forall a. ItemsSlot -> Items.Query a -> Algebra Unit
itemsPeek' _ (Items.Add _ _) = resort
itemsPeek' _ (Items.Adds _ _) = resort
itemsPeek' _ _ = pure unit

itemPeek :: forall a. Items.ItemSlot -> Item.Query a -> Algebra Unit
itemPeek slot (Item.Open _) = do
  mbit <- query' cpItems ItemsSlot $ right $ ChildF slot $ request Item.GetItem
  case mbit of
    Nothing -> pure unit
    Just it -> do
      state <- get
      liftEff $ openItem it (state ^. _sort) (state ^. _salt)
itemPeek slot (Item.Configure _) = do
  mbit <- query' cpItems ItemsSlot $ right $ ChildF slot $ request Item.GetItem
  case mbit of
    Nothing -> pure unit
    Just (PhantomItem _) -> pure unit
    Just item -> do
      mbURI <- map join
               $ query' cpItems ItemsSlot $ right $ ChildF slot $ request Item.GetURI
      configure mbURI (itemResource item)

itemPeek slot (Item.ConfigureView _) = do
  mbit <- query' cpItems ItemsSlot $ right $ ChildF slot $ request Item.GetItem
  case mbit of
    Nothing -> pure unit
    Just (PhantomItem _) -> pure unit
    Just item -> do
      (State state) <- get
      viewInfo <- liftAff $ Auth.authed $ API.viewInfo $ itemResource item
      query' cpDialog DialogSlot $ left $ action
        $ Dialog.Show (Dialog.SQLMount state.path)
      forceRerender'
      querySQLMount state $ SQLMount.Reload viewInfo
      querySQLMount state
        $ SQLMount.UpdateName $ R.resourceName $ itemResource item
  where
  querySQLMount state q =
    void
    $ query' cpDialog DialogSlot
    $ right
    $ ChildF (injSlot Dialog.cpSQLMount (Dialog.SQLMountSlot state.path))
    $ injQuery Dialog.cpSQLMount
    $ left
    $ action q


itemPeek _ (Item.Move _) = do
  mbit <- query' cpItems ItemsSlot $ right $ ChildF slot $ request Item.GetItem
  case mbit of
    Nothing -> pure unit
    Just it -> void do
      query' cpDialog DialogSlot $ left $ action
        $ Dialog.Show (Dialog.Rename $ itemResource it)
      forceRerender'
      flip getDirectories rootDir \x ->
        void $ query' cpDialog DialogSlot $ right
        $ ChildF (injSlot Dialog.cpRename (itemResource it))
        $ injQuery Dialog.cpRename (action $ Rename.AddDirs x)

itemPeek _ (Item.Remove _) = do
  mbit <- query' cpItems ItemsSlot $ right $ ChildF slot $ request Item.GetItem
  case mbit of
    Nothing -> pure unit
    Just item -> void do
      let r = itemResource item
      mbTrashFolder <- liftAff $ Auth.authed $ API.delete r
      query' cpItems ItemsSlot $ left $ action
        $ Items.Filter (not <<< eq r <<< itemResource)
      maybe (pure unit) (\x -> void $ query' cpItems ItemsSlot $ left $ action
                               $ Items.Add $ Item x) mbTrashFolder
itemPeek _ (Item.Share _) = do
  mbit <- query' cpItems ItemsSlot $ right $ ChildF slot $ request Item.GetItem
  case mbit of
    Nothing -> pure unit
    Just item -> void do
      loc <- liftEff locationString
      state <- get
      let url = loc <> "/"
                <> itemURL (state ^. _sort) (state ^. _salt) ReadOnly item
      query' cpDialog DialogSlot $ left $ action
        $ Dialog.Show (Dialog.Share url)
itemPeek _ (Item.Download _) = do
  mbit <- query' cpItems ItemsSlot $ right $ ChildF slot $ request Item.GetItem
  case mbit of
    Nothing -> pure unit
    Just it -> download $ itemResource it
itemPeek _ _ = pure unit

searchPeek :: forall a. SearchSlot -> Search.Query a -> Algebra Unit
searchPeek _ (Search.Clear _) = do
  salt <- liftEff newSalt
  (State state) <- get
  liftEff $ setLocation
    $ browseURL Nothing state.sort salt state.path
searchPeek _ _ = pure unit

resort :: Algebra Unit
resort = do
  sort <- gets (^. _sort)
  mbIsSearching <- query' cpSearch SearchSlot (request Search.IsSearching)
  case mbIsSearching of
    Nothing -> pure unit
    Just isSearching ->
      void $ query' cpItems ItemsSlot $ left
      $ action (Items.SortBy $ sortItem isSearching sort)

configure :: Maybe String -> R.Resource -> Algebra Unit
configure Nothing _ = pure unit
configure (Just info) res@(R.Database _) = do
  void case runParseAbsoluteURI info of
    Left err -> do
      query' cpDialog DialogSlot $ left $ action
        $ Dialog.Show (Dialog.Error
                       $ "There was a problem reading the mount settings: "
                       <>  show err)
    Right uri -> do
      let parent = R.resourceDir res
          r = Mount.mountDialogFromURI res uri
      b <- query' cpDialog DialogSlot $ left $ action
        $ Dialog.Show (Dialog.Mount parent)
      forceRerender'
      a <- query' cpDialog DialogSlot $ right
        $ ChildF (injSlot Dialog.cpMount $ Dialog.MountSlot parent)
        $ injQuery Dialog.cpMount (action $ Mount.ModifyState (const r))
      pure $ pure unit
configure _ _ = pure unit

download :: R.Resource -> Algebra Unit
download res = do
  query' cpDialog DialogSlot $ left $ action
    $ Dialog.Show (Dialog.Download res)
  forceRerender'
  getChildren (const true)
    (\x -> void $ query' cpDialog DialogSlot $ right
           $ ChildF (injSlot Dialog.cpDownload res)
           $ injQuery Dialog.cpDownload (action $ Download.AddSources x))
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
getDirectories = getChildren (\x -> R.isDirectory x || R.isDatabase x)
