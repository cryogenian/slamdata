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

module SlamData.Quasar.FS
  ( children
  , transitiveChildrenProducer
  , getNewName
  , move
  , listing
  , delete
  , messageIfFileNotFound
  , dirNotAccessible
  , fileNotAccessible
  , module Quasar.Error
  ) where

import SlamData.Prelude

import Control.Coroutine as CR
import Control.Monad.Aff as Aff
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class as MR

import Data.Array as Arr
import Data.Foldable as F
import Data.Lens ((.~), (^.), (^?), _Left)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as Set
import Data.String as S

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (QError)
import Quasar.FS as QFS
import Quasar.FS.Resource as QR
import Quasar.Types (AnyPath, DirPath, FilePath)

import SlamData.Config as Config
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Aff (QEff, runQuasarF, Wiring)

import Utils.AffableProducer as AP
import Utils.Completions (memoizeCompletionStrs)

children
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → m (Either QError (Array R.Resource))
children wiring dir = runExceptT do
  cs ← ExceptT $ listing wiring dir
  let result = (R._root .~ dir) <$> cs
  lift $ fromAff $ memoizeCompletionStrs dir result
  pure result

-- | Produces a stream of the transitive children of a path
transitiveChildrenProducer
  ∷ ∀ r eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → CR.Producer (Array R.Resource) m Unit
transitiveChildrenProducer wiring dirPath = do
  AP.produce \emit → do
    activeRequests ← Ref.newRef $ Set.singleton $ P.printPath dirPath
    void $ Aff.runAff Exn.throwException (const (pure unit)) $
      go emit activeRequests dirPath
  where
  go emit activeRequests start = do
    let strPath = P.printPath start
    eitherChildren ← children wiring start
    liftEff $ Ref.modifyRef activeRequests $ Set.delete strPath
    for_ eitherChildren \items → do
      liftEff $ emit $ Left items
      let parents = Arr.mapMaybe (either Just (const Nothing) ∘ R.getPath) items
      for_ parents $ \p →
        liftEff $ Ref.modifyRef activeRequests $ Set.insert $ P.printPath p
      for_ parents $ go emit activeRequests
    remainingRequests ← liftEff $ Ref.readRef activeRequests
    when (Set.isEmpty remainingRequests) $
      liftEff ∘ emit $ Right unit

listing
  ∷ ∀ r eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → m (Either QError (Array R.Resource))
listing wiring p =
  map (map toResource) <$> runQuasarF wiring (QF.dirMetadata p)
  where
  toResource ∷ QFS.Resource → R.Resource
  toResource res = case res of
    QFS.File path → R.File path
    QFS.Directory path →
      let workspaceName
            = S.stripSuffix ("." <> Config.workspaceExtension)
            ∘ P.runDirName =<< P.dirName path
      in case workspaceName of
        Just name → R.Workspace (p </> P.dir name)
        Nothing → R.Directory path
    QFS.Mount (QFS.MongoDB path) → R.Mount (R.Database path)
    QFS.Mount (QFS.View path) → R.Mount (R.View path)

-- | Generates a new resource name based on a directory path and a name for the
-- | resource. If the name already exists in the path a number is appended to
-- | the end of the name.
getNewName
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → String
  → m (Either QError String)
getNewName wiring parent name = do
  result ← runQuasarF wiring (QF.dirMetadata parent)
  pure case result of
    Right items
      | exists name items → Right (getNewName' items 1)
      | otherwise → Right name
    Left QF.NotFound → Right name
    Left err → Left err
  where

  getNewName' ∷ Array QFS.Resource → Int → String
  getNewName' items i =
    let arr = S.split "." name
    in fromMaybe "" do
      body ← Arr.head arr
      suffixes ← Arr.tail arr
      let newName = S.joinWith "." $ Arr.cons (body <> " " <> show i) suffixes
      pure if exists newName items
           then getNewName' items (i + one)
           else newName

  exists ∷ String → Array QFS.Resource → Boolean
  exists name = F.any ((_ == name) ∘ printName ∘ QR.getName)

  printName ∷ Either (Maybe P.DirName) P.FileName → String
  printName = either (fromMaybe "" ∘ map P.runDirName) P.runFileName

-- | Will return `Just` in case the resource was successfully moved, and
-- | `Nothing` in case no resource existed at the requested source path.
move
  ∷ ∀ r eff m
  . (Monad m, MR.MonadRec m, Affable (QEff eff) m)
  ⇒ Wiring r
  → R.Resource
  → AnyPath
  → m (Either QError (Maybe AnyPath))
move wiring src tgt = do
  let srcPath = R.getPath src
  runExceptT ∘ traverse (cleanViewMounts wiring) $ srcPath ^? _Left
  result ←
    runQuasarF wiring case src of
      R.Mount _ → QF.moveMount srcPath tgt
      _ → QF.moveData srcPath tgt
  pure
    case result of
      Right _ → Right (Just tgt)
      Left QF.NotFound → Right Nothing
      Left err → Left err

delete
  ∷ ∀ r eff m
  . (Monad m, MR.MonadRec m, Affable (QEff eff) m)
  ⇒ Wiring r
  → R.Resource
  → m (Either QError (Maybe R.Resource))
delete wiring resource =
  runExceptT $
    if R.isMount resource || alreadyInTrash resource
    then
      forceDelete wiring resource $> Nothing
    else
      moveToTrash resource `catchError` \(err ∷ QError) →
        forceDelete wiring resource $> Nothing

  where
  msg ∷ String
  msg = "cannot delete"

  moveToTrash
    ∷ R.Resource
    → ExceptT QError m (Maybe R.Resource)
  moveToTrash res = do
    let
      d = (res ^. R._root) </> P.dir Config.trashFolder
      path = (res # R._root .~ d) ^. R._path
    name ← ExceptT $ getNewName wiring d (res ^. R._name)
    ExceptT $ move wiring res (path # R._nameAnyPath .~ name)
    pure ∘ Just $ R.Directory d

  alreadyInTrash ∷ R.Resource → Boolean
  alreadyInTrash res =
    case res ^. R._path of
      Left path → alreadyInTrash' path
      Right _ → alreadyInTrash' (res ^. R._root)

  alreadyInTrash' ∷ DirPath → Boolean
  alreadyInTrash' d =
    if d == P.rootDir
    then false
    else maybe false go $ P.peel d

    where
    go ∷ Tuple DirPath (Either P.DirName P.FileName) → Boolean
    go (Tuple d name) =
      case name of
        Right _ → false
        Left n →
          if n == P.DirName Config.trashFolder
          then true
          else alreadyInTrash' d

forceDelete
  ∷ ∀ r eff m
  . (Monad m, MR.MonadRec m, Affable (QEff eff) m)
  ⇒ Wiring r
  → R.Resource
  → ExceptT QError m Unit
forceDelete wiring res =
  case res of
    R.Mount _ →
      ExceptT ∘ runQuasarF wiring $ QF.deleteMount (R.getPath res)
    _ → do
      let path = R.getPath res
      traverse (cleanViewMounts wiring) $ path ^? _Left
      ExceptT ∘ runQuasarF wiring $ QF.deleteData path

cleanViewMounts
  ∷ ∀ r eff m
  . (Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → ExceptT QError m Unit
cleanViewMounts wiring path =
  hoistExceptT fromAff $ CR.runProcess (producer CR.$$ consumer)

  where

  hoistExceptT
    ∷ ∀ e a
    . (Aff.Aff (QEff eff) ~> m)
    → ExceptT e (Aff.Aff (QEff eff)) a
    → ExceptT e m a
  hoistExceptT nat (ExceptT m) = ExceptT (nat m)

  producer ∷ CR.Producer (Array R.Resource) (ExceptT QError (Aff.Aff (QEff eff))) Unit
  producer =
    FT.hoistFreeT lift $
      transitiveChildrenProducer wiring path

  consumer ∷ CR.Consumer (Array R.Resource) (ExceptT QError (Aff.Aff (QEff eff))) Unit
  consumer =
    CR.consumer \fs → do
      traverse_ deleteViewMount fs
      pure Nothing

  deleteViewMount
    ∷ R.Resource
    → ExceptT QError (Aff.Aff (QEff eff)) Unit
  deleteViewMount =
    case _ of
      R.Mount (R.View vp) →
        ExceptT ∘ runQuasarF wiring $ QF.deleteMount (Right vp)
      _ → pure unit

messageIfFileNotFound
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → FilePath
  → String
  → m (Either QError (Maybe String))
messageIfFileNotFound wiring path defaultMsg =
  handleResult <$> runQuasarF wiring (QF.fileMetadata path)
  where
  handleResult ∷ ∀ a. Either QF.QError a → Either QError (Maybe String)
  handleResult (Left QF.NotFound) = Right (Just defaultMsg)
  handleResult (Left err) = Left err
  handleResult (Right _) = Right Nothing

dirNotAccessible
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → m (Maybe QF.QError)
dirNotAccessible wiring path =
  either Just (const Nothing) <$> runQuasarF wiring (QF.dirMetadata path)

fileNotAccessible
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → FilePath
  → m (Maybe QF.QError)
fileNotAccessible wiring path =
  either Just (const Nothing) <$> runQuasarF wiring (QF.fileMetadata path)
