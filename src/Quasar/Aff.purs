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

module Quasar.Aff
  ( children
  , mountInfo
  , viewInfo
  , move
  , reqHeadersToJSON
  , saveMount
  , delete
  , getNewName
  , makeFile
  , ldJSON

  , fields
  , templated
  , all
  , sample
  , transitiveChildrenProducer
  , query
  , queryPrecise
  , count

  , save
  , load
  , messageIfFileNotFound
  , viewQuery
  , fileQuery

  , retrieveAuthProviders
  , compile
  , RetryEffects
  , encodeURI
  ) where

import SlamData.Prelude

import Control.Apply (lift2)
import Control.Coroutine as CR
import Control.Coroutine.Aff as ACR
import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Error.Class as Err
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Trans (runReaderT)

import Data.Argonaut ((~>), (:=))
import Data.Argonaut as JS
import Data.Array as Arr
import Data.Date as Date
import Data.Foldable as F
import Data.Lens ((.~), (^.))
import Data.List as L
import Data.MediaType (MediaType(..), mediaTypeToString)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as Set
import Data.String as S
import Data.StrMap as SM

import DOM (DOM)

import Global (encodeURIComponent)

import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))

import OIDCCryptUtils.Types as OIDC

import Quasar.Advanced.Auth (PermissionToken) as Auth
import Quasar.Advanced.Auth.Provider (Provider) as Auth
import Quasar.Advanced.QuasarAF as QF
import Quasar.Advanced.QuasarAF.Interpreter.Aff as QFA
import Quasar.Data (QData(..), JSONMode(..))
import Quasar.Error (lowerQError)
import Quasar.FS as QFS
import Quasar.FS.Resource as QR
import Quasar.Mount as QM
import Quasar.Mount.MongoDB as QMountMDB
import Quasar.Mount.View as QMountV

import SlamData.Config as Config
import SlamData.FileSystem.Resource as R

import Utils.Completions (memoizeCompletionStrs)
import Utils.Path as PU

-- | Runs a `QuasarF` request in `Aff`, using the `QError` type for errors that
-- | may arise, which allows for convenient catching of 404 errors.
runQuasarF'
  ∷ ∀ eff a
  . Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → QF.QuasarAFP (Either QF.QError a)
  → Aff (ajax ∷ AX.AJAX | eff) (Either QF.QError a)
runQuasarF' idToken permissions qf =
  runReaderT (QFA.eval qf) { basePath: "", idToken, permissions }

-- | Runs a `QuasarF` request in `Aff`, using the standard `Error` type for
-- | errors thay may arise.
runQuasarF
  ∷ ∀ eff a
  . Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → QF.QuasarAFP (Either QF.QError a)
  → Aff (ajax ∷ AX.AJAX | eff) (Either Exn.Error a)
runQuasarF idToken permissions qf =
  lmap lowerQError <$> runQuasarF' idToken permissions qf

type QEff eff = RetryEffects (ajax ∷ AX.AJAX | eff)
type RetryEffects eff = (avar ∷ AVar.AVAR, ref ∷ Ref.REF, now ∷ Date.Now | eff)

children
  ∷ ∀ eff
  . PU.DirPath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff (dom ∷ DOM | eff)) (Either Exn.Error (Array R.Resource))
children dir idToken perms = runExceptT do
  cs ← ExceptT $ listing dir idToken perms
  let result = (R._root .~ dir) <$> cs
  lift $ memoizeCompletionStrs dir result
  pure result

listing
  ∷ ∀ eff
  . PU.DirPath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error (Array R.Resource))
listing p idToken perms =
  map (map toResource) <$> runQuasarF idToken perms (QF.dirMetadata p)
  where
  toResource ∷ QFS.Resource → R.Resource
  toResource res = case res of
    QFS.File path → R.File path
    QFS.Directory path →
      let notebookName
            = S.stripSuffix ("." <> Config.notebookExtension)
            <<< P.runDirName =<< P.dirName path
      in case notebookName of
        Just name → R.Notebook (p </> P.dir name)
        Nothing → R.Directory path
    QFS.Mount (QFS.MongoDB path) → R.Mount (R.Database path)
    QFS.Mount (QFS.View path) → R.Mount (R.View path)

makeFile
  ∷ ∀ eff
  . PU.FilePath
  → QData
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error Unit)
makeFile path content idToken perms =
  runQuasarF idToken perms $ QF.writeFile path content

encodeURI ∷ String → String
encodeURI str =
  let
    str' = fromMaybe str $ S.stripPrefix "." str

    encode ∷ String → String
    encode = encodeURIComponent

    qmSplitted ∷ Array String
    qmSplitted = S.split "?" str'

    ampSplitted ∷ Maybe (Array String)
    ampSplitted = map (S.split "&") $ qmSplitted Arr.!! 1

    eqSplitted ∷ Maybe (Array (Array String))
    eqSplitted = map (map $ S.split "=") $ ampSplitted

    maybeModify ∷ ∀ a. Int → (a → a) → Array a → Array a
    maybeModify ix fn arr =
      fromMaybe arr $ Arr.modifyAt ix fn arr

    eqSplittedEncoded ∷ Maybe (Array (Array String))
    eqSplittedEncoded = map (map (maybeModify 1 encode)) eqSplitted

    eqMerged ∷ Maybe (Array String)
    eqMerged = map (map (S.joinWith "=")) eqSplittedEncoded

    ampMerged ∷ Maybe String
    ampMerged = map (S.joinWith "&") eqMerged

    slashSplitted ∷ Maybe (Array String)
    slashSplitted = map (S.split "/") $ Arr.head qmSplitted

    slashSplittedEncoded ∷ Maybe (Array String)
    slashSplittedEncoded = map (map encode) $ slashSplitted

    slashMerged ∷ Maybe String
    slashMerged = map (S.joinWith "/") slashSplittedEncoded

    afterQM ∷ String
    afterQM = foldMap ("?" ⊕ _) ampMerged

    beforeQM ∷ String
    beforeQM = fromMaybe "" slashMerged

  in
    beforeQM ⊕ afterQM

reqHeadersToJSON ∷ Array RequestHeader → JS.Json
reqHeadersToJSON = foldl go JS.jsonEmptyObject
  where
  go obj (Accept mime) = "Accept" := mediaTypeToString mime ~> obj
  go obj (ContentType mime) = "Content-Type" := mediaTypeToString mime ~> obj
  go obj (RequestHeader k v) = k := v ~> obj

mountInfo
  ∷ ∀ eff
  . PU.DirPath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error QMountMDB.Config)
mountInfo path idToken perms = runExceptT do
  result ← ExceptT $ runQuasarF idToken perms $ QF.getMount (Left path)
  case result of
    QM.MongoDBConfig config → pure config
    _ → Err.throwError $ Exn.error $
      P.printPath path <> " is not a MongoDB mount point"

viewInfo
  ∷ ∀ eff
  . PU.FilePath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error QMountV.Config)
viewInfo path idToken perms = runExceptT do
  result ← ExceptT $ runQuasarF idToken perms $ QF.getMount (Right path)
  case result of
    QM.ViewConfig config → pure config
    _ → Err.throwError $ Exn.error $ P.printPath path <> " is not an SQL² view"

-- | Generates a new resource name based on a directory path and a name for the
-- | resource. If the name already exists in the path a number is appended to
-- | the end of the name.
getNewName
  ∷ ∀ eff
  . PU.DirPath
  → String
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error String)
getNewName parent name idToken perms = do
  result ← runQuasarF' idToken perms (QF.dirMetadata parent)
  pure case result of
    Left (QF.Error err) → Left err
    Right items | exists name items → Right (getNewName' items 1)
    _ → Right name
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
  exists name = F.any ((_ == name) <<< printName <<< QR.getName)

  printName ∷ Either (Maybe P.DirName) P.FileName → String
  printName = either (fromMaybe "" <<< map P.runDirName) P.runFileName

-- | Will return `Just` in case the resource was successfully moved, and
-- | `Nothing` in case no resource existed at the requested source path.
move
  ∷ ∀ eff
  . R.Resource
  → PU.AnyPath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff (dom ∷ DOM | eff)) (Either Exn.Error (Maybe PU.AnyPath))
move src tgt idToken perms = do
  either
    (\dir → cleanViewMounts dir idToken perms)
    (const (pure (Right unit)))
    (R.getPath src)
  result <- runQuasarF' idToken perms case src of
    R.Mount _ → QF.moveMount (R.getPath src) tgt
    _ → QF.moveData (R.getPath src) tgt
  pure case result of
    Right _ → Right (Just tgt)
    Left QF.NotFound → Right Nothing
    Left (QF.Error err) → Left err

saveMount
  ∷ ∀ eff
  . PU.DirPath
  → QMountMDB.Config
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error Unit)
saveMount path config idToken perms =
  runQuasarF idToken perms $
    QF.updateMount (Left path) (QM.MongoDBConfig config)

delete
  ∷ ∀ eff
  . R.Resource
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff (dom ∷ DOM | eff)) (Either Exn.Error (Maybe R.Resource))
delete resource idToken perms =
  if not (R.isMount resource || alreadyInTrash resource)
  then moveToTrash resource <|> forceDelete resource idToken perms $> pure Nothing
  else forceDelete resource idToken perms $> pure Nothing

  where
  msg ∷ String
  msg = "cannot delete"

  moveToTrash
    ∷ R.Resource
    → Aff (QEff (dom ∷ DOM | eff)) (Either Exn.Error (Maybe R.Resource))
  moveToTrash res = runExceptT do
    let d = (res ^. R._root) </> P.dir Config.trashFolder
        path = (res # R._root .~ d) ^. R._path
    name ← ExceptT $ getNewName d (res ^. R._name) idToken perms
    ExceptT $ move res (path # R._nameAnyPath .~ name) idToken perms
    pure $ Just $ R.Directory d

  alreadyInTrash ∷ R.Resource → Boolean
  alreadyInTrash res =
    case res ^. R._path of
      Left path → alreadyInTrash' path
      Right _ → alreadyInTrash' (res ^. R._root)

  alreadyInTrash' ∷ PU.DirPath → Boolean
  alreadyInTrash' d =
    if d == P.rootDir
    then false
    else maybe false go $ P.peel d

    where
    go ∷ Tuple PU.DirPath (Either P.DirName P.FileName) → Boolean
    go (Tuple d name) =
      case name of
        Right _ → false
        Left n →
          if n == P.DirName Config.trashFolder
          then true
          else alreadyInTrash' d

forceDelete
  ∷ ∀ eff
  . R.Resource
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff (dom ∷ DOM | eff)) (Either Exn.Error Unit)
forceDelete res idToken perms = case res of
  R.Mount _ → runQuasarF idToken perms $ QF.deleteMount (R.getPath res)
  _ → do
    let path = R.getPath res
    either
      (\dir → cleanViewMounts dir idToken perms)
      (const (pure (Right unit)))
      path
    runQuasarF idToken perms $ QF.deleteData path

cleanViewMounts
  ∷ ∀ eff
  . PU.DirPath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff (dom ∷ DOM | eff)) (Either Exn.Error Unit)
cleanViewMounts path idToken perms = runExceptT do
  rs ← ExceptT $ children path idToken perms
  lift $ traverse_ deleteViewMount rs
  where
  deleteViewMount (R.Mount (R.View vp)) = runQuasarF idToken perms $ QF.deleteMount (Right vp)
  deleteViewMount _ = pure $ pure unit

ldJSON ∷ MediaType
ldJSON = MediaType "application/ldjson"

-- | Produces a stream of the transitive children of a path
transitiveChildrenProducer
  ∷ ∀ eff
  . PU.DirPath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → CR.Producer
      (Array R.Resource)
      (Aff (QEff (err ∷ Exn.EXCEPTION, dom ∷ DOM | eff)))
      Unit
transitiveChildrenProducer dirPath idToken perms = do
  ACR.produce \emit → do
    activeRequests ← Ref.newRef $ Set.singleton $ P.printPath dirPath
    Aff.runAff Exn.throwException (const (pure unit)) $ go emit activeRequests dirPath
  where
  go emit activeRequests start = do
    let strPath = P.printPath start
    eitherChildren ← children start idToken perms
    liftEff $ Ref.modifyRef activeRequests $ Set.delete strPath
    for_ eitherChildren \items → do
      liftEff $ emit $ Left items
      let parents = Arr.mapMaybe (either Just (const Nothing) <<< R.getPath) items
      for_ parents $ \p →
        liftEff $ Ref.modifyRef activeRequests $ Set.insert $ P.printPath p
      for_ parents $ go emit activeRequests
    remainingRequests ← liftEff $ Ref.readRef activeRequests
    if Set.isEmpty remainingRequests
      then liftEff $ emit $ Right unit
      else pure unit

-- | This is template string where actual path is encoded like {{path}}
type SQL = String

query
  ∷ ∀ eff
  . PU.DirPath
  → SQL
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either String JS.JArray)
query path sql idToken perms = do
  lmap QF.printQError <$>
    runQuasarF' idToken perms (QF.readQuery Readable path sql SM.empty Nothing)

queryPrecise
  ∷ ∀ eff
  . PU.DirPath
  → SQL
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either String JS.JArray)
queryPrecise path sql idToken perms = do
  lmap QF.printQError <$>
    runQuasarF' idToken perms (QF.readQuery Precise path sql SM.empty Nothing)

count
  ∷ ∀ eff
  . PU.FilePath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error Int)
count file idToken perms = runExceptT do
  let backendPath = fromMaybe P.rootDir (P.parentDir file)
      sql = templated file "SELECT COUNT(*) as total FROM {{path}}"
  result ← ExceptT $ runQuasarF idToken perms $
    QF.readQuery Readable backendPath sql SM.empty Nothing
  pure $ fromMaybe 0 (readTotal result)
  where
  readTotal ∷ JS.JArray → Maybe Int
  readTotal =
    Data.Int.fromNumber
      <=< JS.toNumber
      <=< SM.lookup "total"
      <=< JS.toObject
      <=< Arr.head

messageIfFileNotFound
  ∷ ∀ eff
  . PU.FilePath
  → String
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error (Maybe String))
messageIfFileNotFound path defaultMsg idToken perms =
  handleResult <$> runQuasarF' idToken perms (QF.fileMetadata path)
  where
  handleResult ∷ ∀ a. Either QF.QError a → Either Exn.Error (Maybe String)
  handleResult (Left (QF.Error e)) = Left e
  handleResult (Left QF.NotFound) = Right (Just defaultMsg)
  handleResult (Right _) = Right Nothing

-- | Compiles a query.
-- |
-- | If a file path is provided for the input path the query can use the
-- | {{path}} template syntax to have the file's path inserted, and the file's
-- | parent directory will be used to determine the backend to use in Quasar.
compile
  ∷ ∀ eff
  . PU.AnyPath
  → SQL
  → SM.StrMap String
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error String)
compile path sql varMap idToken perms = runExceptT do
  let backendPath = either id (fromMaybe P.rootDir <<< P.parentDir) path
      sql' = maybe sql (flip templated sql) $ either (const Nothing) Just path
  result ← ExceptT $ runQuasarF idToken perms $
    QF.compileQuery backendPath sql' varMap
  case S.stripPrefix "MongoDB\n" result of
    Nothing → Err.throwError $ Exn.error "Incorrect compile response"
    Just plan → pure plan

-- | Runs a query creating a view mount for the query.
-- |
-- | If a file path is provided for the input path the query can use the
-- | {{path}} template syntax to have the file's path inserted.
viewQuery
  ∷ ∀ eff
  . PU.AnyPath
  → PU.FilePath
  → SQL
  → SM.StrMap String
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error Unit)
viewQuery path dest sql vars idToken perms =
  runQuasarF idToken perms $ QF.updateMount (Right dest) $ QM.ViewConfig
    { query: maybe sql (flip templated sql) $ either (const Nothing) Just path
    , vars
    }

-- | Runs a query for a particular file (the query can use the {{path}} template
-- | syntax to have the file's path inserted), writing the results to a file.
-- | The query backend will be determined by the input file path.
-- |
-- | The returned value is the output path returned by Quasar. For some queries
-- | this will be the input file rather than the specified destination.
fileQuery
  ∷ ∀ eff
  . PU.FilePath
  → PU.FilePath
  → SQL
  → SM.StrMap String
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error PU.FilePath)
fileQuery file dest sql vars idToken perms =
  let backendPath = fromMaybe P.rootDir (P.parentDir file)
  in runQuasarF idToken perms $
    map _.out <$> QF.writeQuery backendPath dest (templated file sql) vars

sample
  ∷ ∀ eff
  . PU.FilePath
  → Int
  → Int
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error JS.JArray)
sample file offset limit idToken perms =
  runQuasarF idToken perms $ QF.readFile Readable file (Just { limit, offset })

all
  ∷ ∀ eff
  . PU.FilePath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error JS.JArray)
all file idToken perms =
  runQuasarF idToken perms $ QF.readFile Readable file Nothing

templated ∷ PU.FilePath → SQL → SQL
templated res = S.replace "{{path}}" ("`" <> P.printPath res <> "`")

fields
  ∷ ∀ eff
  . PU.FilePath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error (Array String))
fields file idToken perms = runExceptT do
  jarr ← ExceptT $ sample file 0 100 idToken perms
  case jarr of
    [] → Err.throwError $ Exn.error "empty file"
    _ → pure $ Arr.nub $ getFields =<< jarr

  where
  -- The output of this function is mysterious, but luckily is used in just one place.
  --
  -- TODO: Rather than accumulating a an array of formatted strings, this should be refactored
  -- to return an array of *arrays* of unformatted strings, which can then be formatted by the
  -- client (e.g. to intercalate with dots and add backticks).
  getFields ∷ JS.Json → Array String
  getFields = Arr.filter (_ /= "") <<< Arr.nub <<< go []
    where
    go ∷ Array String → JS.Json → Array String
    go [] json = go [""] json
    go acc json =
      if JS.isObject json
      then maybe acc (goObj acc) $ JS.toObject json
      else if JS.isArray json
           then maybe acc (goArr acc) $ JS.toArray json
           else acc

      where
      goArr ∷ Array String → JS.JArray → Array String
      goArr acc arr =
        Arr.concat $ go (lift2 append acc $ mkArrIxs arr) <$> arr
        where
        mkArrIxs ∷ JS.JArray → Array String
        mkArrIxs jarr =
          map (\x → "[" <> show x <> "]") $ Arr.range 0 $ Arr.length jarr - 1

      goObj ∷ Array String → JS.JObject → Array String
      goObj acc = Arr.concat <<< map (goTuple acc) <<< L.fromList <<< SM.toList

      goTuple ∷ Array String → Tuple String JS.Json → Array String
      goTuple acc (Tuple key json) =
        go ((\x → x <> ".`" <> key <> "`") <$> acc) json

-- | Saves a single JSON value to a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
save
  ∷ ∀ eff
  . PU.FilePath
  → JS.Json
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either Exn.Error Unit)
save path json idToken perms =
  runQuasarF idToken perms $ QF.writeFile path (JSON Readable [json])

-- | Loads a single JSON value from a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
load
  ∷ ∀ eff
  . PU.FilePath
  → Maybe OIDC.IdToken
  → Array Auth.PermissionToken
  → Aff (QEff eff) (Either String JS.Json)
load file idToken perms =
  runQuasarF' idToken perms (QF.readFile Readable file Nothing) <#> case _ of
    Right [file] → Right file
    Right _ → Left "Unexpected result when loading value from file"
    Left err → Left (QF.printQError err)

-- | Returns `Nothing` in case the authorization service is not available, and `Just` in case
-- | Quasar responded with a valid array of OIDC providers.
retrieveAuthProviders
  ∷ ∀ eff
  . Aff (QEff eff) (Either Exn.Error (Maybe (Array Auth.Provider)))
retrieveAuthProviders =
  runQuasarF' Nothing [] QF.authProviders <#> case _ of
    Left (QF.Error err) → Left err
    Left QF.NotFound → Right Nothing
    Right providers → Right (Just providers)
