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
  , transitiveChildren
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

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Fork (class MonadFork)

import Data.Array as Arr
import Data.Foldable as F
import Data.Lens ((.~), (^.), (^?), _Left)
import Data.List as List
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.String as S

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (QError(..))
import Quasar.FS as QFS
import Quasar.FS.Mount as QFSM
import Quasar.FS.Resource as QR
import Quasar.Types (AnyPath, DirPath, FilePath)

import SlamData.Config as Config
import SlamData.Effects (SlamDataEffects)
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SlamData.Quasar.Data as QD

import SlamData.Workspace.Legacy (isLegacy, loadCompatWorkspace, pruneLegacyData)
import SlamData.Workspace.Model as WM
import SlamData.Workspace.Card.Model as CM

import Utils.Array (enumerate)
import Utils.Ace (RangeRec)

children
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ DirPath
  → m (Either QError (Array R.Resource))
children dir = runExceptT do
  cs ← ExceptT $ listing dir
  let result = (R._root .~ dir) <$> cs
  -- TODO: do this somewhere more appropriate
  -- lift $ fromAff $ memoizeCompletionStrs dir result
  pure result

transitiveChildren
  ∷ ∀ f m
  . (Parallel f m, QuasarDSL m)
  ⇒ DirPath
  → m (Either QError (Array R.Resource))
transitiveChildren start = runExceptT do
  rs ← ExceptT $ children start
  cs ← parTraverse go rs
  pure $ rs <> join cs
  where
  go :: R.Resource → ExceptT QError m (Array R.Resource)
  go r = case R.getPath r of
    Left dir → do
      crs ← ExceptT $ transitiveChildren dir
      pure $ [r] <> crs
    Right _ → pure [r]

listing
  ∷ ∀ m
  . (Functor m, QuasarDSL m)
  ⇒ DirPath
  → m (Either QError (Array R.Resource))
listing p =
  map (map toResource) <$> liftQuasar (QF.dirMetadata p)
  where
  toResource ∷ QFS.Resource → R.Resource
  toResource res = case res of
    QFS.File path → R.File path
    QFS.Directory path →
      let workspaceName
            = S.stripSuffix (S.Pattern $ "." <> Config.workspaceExtension)
            ∘ P.runDirName =<< P.dirName path
      in case workspaceName of
        Just name → R.Workspace (p </> P.dir name)
        Nothing → R.Directory path
    QFS.Mount m → R.Mount $ either R.Database R.View $ QFSM.getPath m

-- | Generates a new resource name based on a directory path and a name for the
-- | resource. If the name already exists in the path a number is appended to
-- | the end of the name.
getNewName
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ DirPath
  → String
  → m (Either QError String)
getNewName parent name = do
  result ← liftQuasar (QF.dirMetadata parent)
  pure case result of
    Right items
      | exists name items → Right (getNewName' items 1)
      | otherwise → Right name
    Left QF.NotFound → Right name
    Left err → Left err
  where

  getNewName' ∷ Array QFS.Resource → Int → String
  getNewName' items i =
    let arr = S.split (S.Pattern ".") name
    in fromMaybe "" do
      body ← Arr.head arr
      suffixes ← Arr.tail arr
      let newName = S.joinWith "." $ Arr.cons (body <> " " <> show i) suffixes
      pure if exists newName items
           then getNewName' items (i + one)
           else newName

  exists ∷ String → Array QFS.Resource → Boolean
  exists n = F.any ((_ == n) ∘ printName ∘ QR.getName)

  printName ∷ Either (Maybe P.DirName) P.FileName → String
  printName = either (fromMaybe "" ∘ map P.runDirName) P.runFileName

-- | Will return `Just` in case the resource was successfully moved, and
-- | `Nothing` in case no resource existed at the requested source path.
move
  ∷ ∀ f m
  . ( MonadAff SlamDataEffects m
    , MonadFork Error m
    , QuasarDSL m
    , Parallel f m
    )
  ⇒ R.Resource
  → AnyPath
  → m (Either QError (Maybe AnyPath))
move src tgt = do
  let
    srcPath = R.getPath src

  runExceptT ∘ traverse cleanViewMounts $ srcPath ^? _Left

  runExceptT case src of
    R.Workspace wsDir → replacePathsInWorkspace wsDir
    _ → pure unit

  result ←
    case src of
      R.Mount _ → liftQuasar $ QF.moveMount srcPath tgt
      _ → liftQuasar $ QF.moveData srcPath tgt

  pure
    case result of
      Right _ → Right (Just tgt)
      Left QF.NotFound → Right Nothing
      Left err → Left err

  where
  replacePathsInWorkspace ∷ DirPath → ExceptT QError m Unit
  replacePathsInWorkspace wsDir = do
    stat × ws ← ExceptT $ loadCompatWorkspace wsDir
    let
      updated =
        ws.cards
          # Map.toList
          # List.mapMaybe (sequence ∘ map (replacePaths wsDir))
          # Map.fromFoldable
      ws' = ws { cards = Map.union updated ws.cards }
    unless (Map.isEmpty updated) $ putWorkspace wsDir ws
    when (isLegacy stat) $ void $ lift $ pruneLegacyData wsDir

  putWorkspace ∷ DirPath → WM.Workspace → ExceptT QError m Unit
  putWorkspace wsDir ws =
    ExceptT $ QD.save (wsDir </> P.file "index") $ WM.encode ws

  replacePaths ∷ DirPath → CM.AnyCardModel → Maybe CM.AnyCardModel
  replacePaths wsDir = case _ of
    CM.Cache m →
      let
        newM =
          S.replace
            (S.Pattern $ P.printPath wsDir)
            (S.Replacement $ either P.printPath P.printPath tgt)
            <$> m
      in
        if newM ≡ m
          then Nothing
          else pure $ CM.Cache newM
    CM.Open r →
      let
        newR = fromMaybe r do
          rel ←
            bisequence
              $ bimap (flip P.relativeTo wsDir) (flip P.relativeTo wsDir)
              $ R.getPath r
          tgtDir ← either Just (const Nothing) tgt
          let
            newP =
              bimap P.canonicalize P.canonicalize
              $ bimap (tgtDir </> _) (tgtDir </> _) rel
          pure $ R.setPath r newP
      in
        if newR ≡ r
          then Nothing
          else pure $ CM.Open newR
    CM.Ace mode m →
      let
        wsStr ∷ String
        wsStr = P.printPath wsDir

        pat ∷ S.Pattern
        pat = S.Pattern wsStr

        tgtStr ∷ String
        tgtStr = either P.printPath P.printPath tgt

        replacement ∷ S.Replacement
        replacement = S.Replacement tgtStr

        srcLen ∷ Int
        srcLen = S.length wsStr

        tgtLen ∷ Int
        tgtLen = S.length tgtStr

        lenDiff ∷ Int
        lenDiff = tgtLen - srcLen

        sortRangeRecs ∷ Array RangeRec → Array RangeRec
        sortRangeRecs =
          Arr.sortBy \a b →
            compare a.startRow b.startRow
            ⊕ compare a.startColumn b.startColumn

        -- We don't care about endings
        eqRangeRec ∷ RangeRec → RangeRec → Boolean
        eqRangeRec a b =
          -- That's a bug workaround, modification of ace field for whatever reason
          -- removes one space left padding :(
          (a.startColumn ≡ b.startColumn
           ∨ a.startColumn + one ≡ b.startColumn
           ∨ a.startColumn - one ≡ b.startColumn)
          ∧ a.startRow ≡ b.startRow

        substrIndices
          ∷ S.Pattern
          → Array (Int × String)
          → Array RangeRec
        substrIndices needle =
          sortRangeRecs ∘ foldl foldRanges [ ]
          where
          findStartColumns ∷ Array Int → String → Array Int
          findStartColumns accum hay =
            case S.indexOf needle hay of
              Nothing → accum
              Just i → findStartColumns (Arr.cons i accum) $ S.drop (i + srcLen) hay

          foldRanges
            ∷ Array RangeRec
            → Int × String
            → Array RangeRec
          foldRanges accum (startRow × hay) =
            let
              starts = findStartColumns [] hay
              fromKWlen = 6
              mkRange start =
                [ { startColumn: start - fromKWlen
                  , endColumn: 0
                  , startRow
                  , endRow: startRow
                  } ]
            in
             foldMap mkRange starts ⊕ accum

        initialRanges ∷ Array RangeRec
        initialRanges = substrIndices pat $ enumerate $ S.split (S.Pattern "\n") m.text

        -- inputs must be sorted
        filterRanges
          ∷ Array RangeRec
          → Array RangeRec
          → Array RangeRec
          → Array RangeRec
        filterRanges accum needles hays = fromMaybe accum do
          needles' ← Arr.uncons needles
          hays' ← Arr.uncons hays
          pure
            $ if eqRangeRec hays'.head needles'.head
            -- Note that we use stuff from hays, because it has endColumn and endRow
              then
                filterRanges
                  (Arr.snoc
                     accum
                     hays'.head{ endColumn = hays'.head.endColumn
                                             + lenDiff
                                             + hays'.head.startRow
                                             - one
                               , startColumn = hays'.head.startColumn
                                               - hays'.head.startRow
                                               - one
                               })
                  needles'.tail
                  hays
              else
                filterRanges
                  (Arr.snoc accum hays'.head)
                  needles
                  hays'.tail

        newText ∷ String
        newText = S.replace pat replacement m.text

        newRanges ∷ Array RangeRec
        newRanges
          | newText ≠ m.text  =
            filterRanges [ ] initialRanges $ sortRangeRecs m.ranges
          | otherwise = m.ranges
      in
        if newText ≡ m.text
          then Nothing
          else pure $ CM.Ace mode { text: newText, ranges: newRanges }

    _ → Nothing

delete
  ∷ ∀ f m
  . ( MonadAff SlamDataEffects m
    , MonadFork Error m
    , QuasarDSL m
    , Parallel f m
    )
  ⇒ R.Resource
  → m (Either QError (Maybe R.Resource))
delete resource =
  runExceptT $
    if R.isMount resource || alreadyInTrash resource
    then
      forceDelete resource $> Nothing
    else
      moveToTrash resource `catchError` \(err ∷ QError) →
        forceDelete resource $> Nothing

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
    name ← ExceptT $ getNewName d (res ^. R._name)
    ExceptT $ move res (path # R._nameAnyPath .~ name)
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
    go (Tuple d' name) =
      case name of
        Right _ → false
        Left n →
          if n == P.DirName Config.trashFolder
          then true
          else alreadyInTrash' d'

forceDelete
  ∷ ∀ f m
  . (QuasarDSL m, Parallel f m)
  ⇒ R.Resource
  → ExceptT QError m Unit
forceDelete res =
  case res of
    R.Mount _ →
      ExceptT ∘ liftQuasar $ QF.deleteMount (R.getPath res)
    _ → do
      let path = R.getPath res
      traverse cleanViewMounts $ path ^? _Left
      ExceptT ∘ liftQuasar $ QF.deleteData path

cleanViewMounts
  ∷ ∀ f m
  . (Parallel f m, QuasarDSL m)
  ⇒ DirPath
  → ExceptT QError m Unit
cleanViewMounts =
  parTraverse_ deleteViewMount <=< ExceptT ∘ transitiveChildren
  where
  deleteViewMount ∷ R.Resource → ExceptT QError m Unit
  deleteViewMount =
    case _ of
      R.Mount (R.View vp) →
        ExceptT ∘ liftQuasar $ QF.deleteMount (Right vp)
      _ → pure unit

messageIfFileNotFound
  ∷ ∀ m
  . (Functor m, QuasarDSL m)
  ⇒ FilePath
  → String
  → m (Either QError (Maybe String))
messageIfFileNotFound path defaultMsg =
  handleResult <$> liftQuasar (QF.fileMetadata path)
  where
  handleResult ∷ ∀ a. Either QF.QError a → Either QError (Maybe String)
  handleResult (Left QF.NotFound) = Right (Just defaultMsg)
  handleResult (Left err) = Left err
  handleResult (Right _) = Right Nothing


dirNotAccessible
  ∷ ∀ m
  . (Functor m, QuasarDSL m)
  ⇒ DirPath
  → m (Maybe QF.QError)
dirNotAccessible path =
  either Just (const Nothing) <$> liftQuasar (QF.dirMetadata path)

fileNotAccessible
  ∷ ∀ m
  . (Functor m, QuasarDSL m)
  ⇒ FilePath
  → m (Maybe QF.QError)
fileNotAccessible path =
  either Just (const Nothing) <$> liftQuasar (QF.fileMetadata path)
